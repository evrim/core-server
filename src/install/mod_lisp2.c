/* -*-C-*-

  ====================================================================
  mod_lisp2 is a rewrite of mod_lisp for Apache2.
  It is based on mod_lisp and the example module from the apache distribution.

  It is distributed under a BSD style license

Copyright 2000-2005 Marc Battyani.
Copyright 2003,2004 Massachusetts Institute of Technology

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. The name of the author may not be used to endorse or promote
      products derived from this software without specific prior
      written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
	    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

  ==================================================================== 
*/

/*
  ====================================================================

This software consists of voluntary contributions made by many
individuals on behalf of the Apache Software Foundation.  For more
information on the Apache Software Foundation, please see
<http://www.apache.org/>.

Portions of this software are based upon public domain software
originally written at the National Center for Supercomputing Applications,
University of Illinois, Urbana-Champaign.

  ====================================================================
*/

/* 
  Change log:

  If request contains chunked data, send chunks (length word plus data)
  to backend lisp. Also eliminated a gcc warning.
  -- Hugh Winkler
     2006-12-05

  Fixed the APR 1.2.2 detection.
  -- sent by several people
     2006-1203

  Fixed u bug that made mod_lisp send all twice to the lisp process.
  -- Lars Rune Nøstdal
     2006-03-14

  Fixes for Apache Portable Runtie (APR) v1.2.2. Should be backward
  compatible at compile time. Must be recompiled to upgrade or 
  downgrade your APR library, though.
  -- Nash Foster <nash@solace.net>
     2006-02-27

  Update for Apache 2.2.0 Compatability
  -- Nikola Vouk
     2006-02-24

  fix r->mtime
  -- Zach Beane
     2006-01-04

  Handle Lisp data correctly if there's no Content-Length header
  -- Dr. Edmund Weitz <edi@agharta.de>
     2005-12-24

  ML_LOG_DEBUG uses the APLOG_DEBUG level, defaults to on, and
  hence the LogLevel directive now controls mod_lisp2 logging.
  Those wishing to squeeze the last iota of performance can use:
     apxs2 -ic -DENABLE_DEBUG=0 mod_lisp2.c
  This eliminates debug calls to the logging API entirely.
  -- Nash Foster <nash@solace.net>
     2005-12-24

  Fixed a declaration for some versions of gcc
  -- Marc Battyani
     2005-08-26

  Set r->mtime directly
  -- Dr. Edmund Weitz <edi@agharta.de>
     2005-06-07

   Read data from Lisp even if it's a HEAD request
   Added "Lisp-Content-Length" header (send after "Content-Length" header to overwrite its value)
   -- Dr. Edmund Weitz <edi@agharta.de>
      2004-12-27

   Fixed lisp_handler to allow for no Content-Length header
   -- Tim Howe <tim.howe@celebrityresorts.com>
      2004-11-15

   Thread safe socket reuse.
   -- Marc Battyani <marc.battyani@fractalconcept.com>
      2004-11-11

   Some minor additions for TBNL.
   -- Dr. Edmund Weitz <edi@agharta.de>
      2004-11-09

   Initial rewrite of mod_lisp for Apache2
   -- Chris Hanson <cph@csail.mit.edu>
      2003-12-02
*/

#define VERSION_STRING "1.3.1"
#define READ_TIMEOUT 60000000

/* Enable debug logging by default so it can be controller with
   Apache's LogLevel directive. */
#if !defined ( ENABLE_DEBUG )
#  define ENABLE_DEBUG    1
#endif

#include "httpd.h"
#include "http_config.h"
#include "http_core.h"
#include "http_log.h"
#include "http_main.h"
#include "http_protocol.h"
#include "http_request.h"
#include "util_script.h"
#include "apr_date.h"
#include "apr_strings.h"
#include "apr_version.h"
#include "apr_errno.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

module AP_MODULE_DECLARE_DATA lisp_module;
/* Work out the version of the apache portable runtime (APR) we're
 * compiling against... with version 1.2.2 some of the interfaces
 * changed a bit. */
#if (APR_MAJOR_VERSION==1 && APR_MINOR_VERSION==2 && APR_PATCH_VERSION>=2)
#define HAVE_APR_1_2_2    1
#endif


#define RELAY_ERROR(expr) do						\
{									\
  while (1)								\
    {									\
      apr_status_t RELAY_ERROR_status = (expr);				\
      if (APR_SUCCESS == RELAY_ERROR_status)				\
	break;								\
      if (!APR_STATUS_IS_EINTR (RELAY_ERROR_status))			\
	return (RELAY_ERROR_status);					\
    }									\
} while (0)

#define ML_LOG_ERROR(status, r, msg)					\
  (ap_log_error (APLOG_MARK, APLOG_ERR, (status),			\
			     		 ((r) -> server), (msg)))

#define ML_LOG_PERROR(r, msg)						\
  ML_LOG_ERROR ((APR_FROM_OS_ERROR (apr_get_os_error ())), (r), (msg))

#define RELAY_HTTP_ERROR(expr) do					\
{									\
  int RELAY_HTTP_ERROR_value = (expr);					\
  if (RELAY_HTTP_ERROR_value != OK)					\
    return (RELAY_HTTP_ERROR_value);					\
} while (0)

#if ENABLE_DEBUG
#  define ML_LOG_DEBUG(r, msg)						\
    (ap_log_error (APLOG_MARK, APLOG_DEBUG, APR_SUCCESS,                \
			       		   ((r) -> server), (msg)))
#else
#  define ML_LOG_DEBUG(r, msg)
#endif

typedef struct lisp_cfg
{
  const char * server_address;
  apr_port_t server_port;
  const char * server_id;
  apr_socket_t * server_socket;
  unsigned int
    server_specified_p : 1,
    server_socket_safe_p : 1;
} lisp_cfg_t;

#define CFG(c) ((lisp_cfg_t *) (c))
#define CFG_REF(c, f) ((c) -> f)
#define SERVER_ADDRESS(c) CFG_REF (c, server_address)
#define SERVER_PORT(c) CFG_REF (c, server_port)
#define SERVER_ID(c) CFG_REF (c, server_id)
#define SERVER_SPECIFIED_P(c) CFG_REF (c, server_specified_p)
#define SERVER_SOCKET(c) CFG_REF (c, server_socket)
#define SERVER_SOCKET_SAFE_P(c) CFG_REF (c, server_socket_safe_p)

typedef struct input_buffer
{
  char data [4096];
  const char * start;
  const char * end;
} input_buffer_t;

typedef const char * header_map_t (const char *);

/* Global variables */
static apr_pool_t * socket_pool = 0;

/* Forward references */
static apr_status_t close_lisp_socket (lisp_cfg_t * cfg);
static apr_status_t copy_headers
  (apr_table_t * table, header_map_t * map_name, apr_socket_t * socket);
static header_map_t map_env_var_to_lisp_header;
static header_map_t map_header_to_lisp_header;
static int write_client_data
  (request_rec * r, const char * data, unsigned int n_bytes);

/* Configuration data */

static lisp_cfg_t *
make_lisp_cfg (apr_pool_t * pool)
{
  return (apr_pcalloc (pool, (sizeof (lisp_cfg_t))));
}

static lisp_cfg_t *
default_lisp_cfg (apr_pool_t * pool)
{
  lisp_cfg_t * cfg = (make_lisp_cfg (pool));
  (SERVER_ADDRESS (cfg)) = (apr_pstrdup (pool, "127.0.0.1"));
  (SERVER_PORT (cfg)) = 3000;
  (SERVER_ID (cfg)) = (apr_pstrdup (pool, "apache"));
  (SERVER_SPECIFIED_P (cfg)) = 0;
  (SERVER_SOCKET (cfg)) = 0;
  (SERVER_SOCKET_SAFE_P (cfg)) = 0;
  return (cfg);
}

static lisp_cfg_t *
copy_lisp_cfg (apr_pool_t * pool, lisp_cfg_t * cfg)
{
  lisp_cfg_t * copy = (make_lisp_cfg (pool));
  (SERVER_ADDRESS (copy)) = (SERVER_ADDRESS (cfg));
  (SERVER_PORT (copy)) = (SERVER_PORT (cfg));
  (SERVER_ID (copy)) = (SERVER_ID (cfg));
  (SERVER_SPECIFIED_P (copy)) = (SERVER_SPECIFIED_P (cfg));
  return (copy);
}

void check_cfg_for_reuse(lisp_cfg_t *local_cfg, lisp_cfg_t *cfg)
{
  if (strcmp((SERVER_ADDRESS (local_cfg)), (SERVER_ADDRESS (cfg))) ||
      (SERVER_PORT (local_cfg)) != (SERVER_PORT (cfg)) ||
      strcmp((SERVER_ID (local_cfg)), (SERVER_ID (cfg))))
    {
      (SERVER_ADDRESS (local_cfg)) = (SERVER_ADDRESS (cfg));
      (SERVER_PORT (local_cfg)) = (SERVER_PORT (cfg));
      (SERVER_ID (local_cfg)) = (SERVER_ID (cfg));
      (SERVER_SPECIFIED_P (local_cfg)) = (SERVER_SPECIFIED_P (cfg));
      (SERVER_SOCKET_SAFE_P (local_cfg)) = 0;
    }
}

#if APR_HAS_THREADS
static apr_threadkey_t *cfg_key;

static lisp_cfg_t *
local_lisp_cfg (lisp_cfg_t *cfg)
{
  void *local_cfg = NULL;
  
  apr_threadkey_private_get(&local_cfg, cfg_key);
  if (local_cfg == NULL)
    {
      local_cfg = copy_lisp_cfg (socket_pool, cfg);
      apr_threadkey_private_set(local_cfg, cfg_key);
      return local_cfg;
    }
  
  check_cfg_for_reuse(local_cfg, cfg);

  return (lisp_cfg_t*) local_cfg;
}
#else
lisp_cfg_t *local_cfg;

local_lisp_cfg (lisp_cfg_t *cfg)
{
    if (!local_cfg)
	local_cfg = copy_lisp_cfg (socket_pool, cfg);

    check_cfg_for_reuse(local_cfg, cfg);

    return local_cfg;
}
#endif

static void *
lisp_create_dir_config (apr_pool_t * pool, char * directory)
{
  return (default_lisp_cfg (pool));
}

static void *
lisp_create_server_config (apr_pool_t * pool, server_rec * s)
{
  return (default_lisp_cfg (pool));
}

static void *
lisp_merge_config (apr_pool_t * pool, void * base_cfg, void * add_cfg)
{
  return
    ((SERVER_SPECIFIED_P (CFG (add_cfg)))
          ? (copy_lisp_cfg (pool, add_cfg))
	       : (SERVER_SPECIFIED_P (CFG (base_cfg)))
	            ? (copy_lisp_cfg (pool, base_cfg))
		         : (default_lisp_cfg (pool)));
}

static const char *
lisp_set_server (cmd_parms * cmd,
                 void * cfg_void,
                 const char * server_address,
                 const char * server_port,
                 const char * server_id)
{
  lisp_cfg_t * cfg = cfg_void;
  long port;
  {
    char * end;
    port = (strtol (server_port, (&end), 0));
    if (((*end) != '\0') || (port < 0))
      return ("malformed server port");
  }
  (SERVER_ADDRESS (cfg)) = (apr_pstrdup ((cmd->pool), server_address));
  (SERVER_PORT (cfg)) = port;
  (SERVER_ID (cfg)) = (apr_pstrdup ((cmd->pool), server_id));
  (SERVER_SPECIFIED_P (cfg)) = 1;
  close_lisp_socket (cfg);
  return (0);
}

/* Socket to Lisp process */

static apr_status_t
open_lisp_socket (lisp_cfg_t * cfg)
{
  apr_sockaddr_t * addr;
  apr_socket_t * socket;

  if ((SERVER_SOCKET (cfg)) != 0)
    {
    if (SERVER_SOCKET_SAFE_P (cfg))
	return (APR_SUCCESS);
      RELAY_ERROR (close_lisp_socket (cfg));
    }

  RELAY_ERROR
    (apr_sockaddr_info_get ((&addr), (cfg->server_address), APR_UNSPEC,
                            (cfg->server_port), 0, socket_pool));

#if (HAVE_APR_1_2_2)
  RELAY_ERROR (apr_socket_create ((&socket), AF_INET, SOCK_STREAM, APR_PROTO_TCP, socket_pool));
#else
  RELAY_ERROR (apr_socket_create ((&socket), AF_INET, SOCK_STREAM, socket_pool));
#endif
  
#if (HAVE_APR_1_2_2)
  RELAY_ERROR (apr_socket_connect (socket, addr));
#else
  RELAY_ERROR (apr_connect (socket, addr));
#endif
  {
    input_buffer_t * buffer
      = (apr_palloc (socket_pool, (sizeof (input_buffer_t))));
    (buffer -> start) = (buffer -> data);
    (buffer -> end) = (buffer -> data);
    RELAY_ERROR (apr_socket_data_set (socket, buffer, "input-buffer", 0));
  }
  (SERVER_SOCKET (cfg)) = socket;
  (SERVER_SOCKET_SAFE_P (cfg)) = 0;
  return (APR_SUCCESS);
}

static apr_status_t
close_lisp_socket (lisp_cfg_t * cfg)
{
  if (SERVER_SOCKET (cfg))
    {
      RELAY_ERROR (apr_socket_close (SERVER_SOCKET (cfg)));
      (SERVER_SOCKET (cfg)) = 0;
    }
  return (APR_SUCCESS);
}

static apr_status_t
write_lisp_data (apr_socket_t * socket,
                 const char * data, unsigned int n_bytes)
{
  const char * p = data;
  apr_size_t n1 = n_bytes;
  while (1)
    {
      apr_size_t n2 = n1;
#if (HAVE_APR_1_2_2)
      RELAY_ERROR (apr_socket_send (socket, p, (&n2)));
#else
      RELAY_ERROR (apr_send (socket, p, &n2));
#endif
      if (n2 == n1)
	return (APR_SUCCESS);
      p += n2;
      n1 -= n2;
    }
}

static apr_status_t
write_lisp_data_chunk (apr_socket_t * socket,
                 const char * data, unsigned int n_bytes)
{
  char crlf[2] =  {0xd, 0xa};
  char length[16];
  snprintf(length, 16, "%x", n_bytes);
  
  apr_status_t status = write_lisp_data (socket, length, strlen(length));
  if ( status == APR_SUCCESS)
    {
      status = write_lisp_data (socket, crlf, 2);
      if ( status == APR_SUCCESS && n_bytes)
	status = write_lisp_data (socket, data, n_bytes);
      if ( status == APR_SUCCESS)
	status = write_lisp_data (socket, crlf, 2);
    }
  return status; 
}

static apr_status_t
write_lisp_line (apr_socket_t * socket, const char * data)
{
  RELAY_ERROR (write_lisp_data (socket, data, (strlen (data))));
  RELAY_ERROR (write_lisp_data (socket, "\n", 1));
  return (APR_SUCCESS);
}

static apr_status_t
write_lisp_header (apr_socket_t * socket,
                   const char * name, const char * value)
{
  RELAY_ERROR (write_lisp_line (socket, name));
  RELAY_ERROR (write_lisp_line (socket, value));
  return (APR_SUCCESS);
}

static apr_status_t
get_input_buffer (apr_socket_t * socket, input_buffer_t ** buffer_r)
{
  return (apr_socket_data_get (((void **) buffer_r), "input-buffer", socket));
}

static apr_status_t
fill_input_buffer (apr_socket_t * socket)
{
  input_buffer_t * buffer;
  apr_size_t length;

  RELAY_ERROR (get_input_buffer (socket, (&buffer)));
#if (HAVE_APR_1_2_2)
  RELAY_ERROR
    (((length = (sizeof (buffer->data))),
      (apr_socket_recv (socket, (buffer->data), (&length)))));
#else
   RELAY_ERROR
     (((length = (sizeof (buffer->data))),
       (apr_recv (socket, (buffer->data), (&length)))));
#endif
  (buffer->start) = (buffer->data);
  (buffer->end) = ((buffer->data) + length);
  if (length == 0)
    (buffer->start) += 1;
  return (APR_SUCCESS);
}

static apr_status_t
read_lisp_line (apr_socket_t * socket, char * s, unsigned int len)
{
  input_buffer_t * buffer;
  char * scan_output = s;
  char * end_output = (scan_output + (len - 1));
  unsigned int n_pending_returns = 0;
  
  RELAY_ERROR (get_input_buffer (socket, (&buffer)));
  while (1)
    {
      if ((buffer->start) == (buffer->end))
	RELAY_ERROR (fill_input_buffer (socket));
      
      if ((buffer->start) > (buffer->end))
	{
	  if (scan_output == s)
	    return (APR_EOF);
	  goto done;
	}

      while (((buffer->start) < (buffer->end)) && (scan_output < end_output))
	{
	  char c = (*(buffer->start)++);
	  if (c == '\n')
	    {
	      if (n_pending_returns > 0)
		n_pending_returns -= 1;
	      goto done;
	    }
	  if (c == '\r')
	    n_pending_returns += 1;
	  else
	    {
	      while ((n_pending_returns > 0) && (scan_output < end_output))
		{
		  (*scan_output++) = '\r';
		  n_pending_returns -= 1;
		}
	      if (scan_output == end_output)
		goto done;
	      (*scan_output++) = c;
	    }
	}
    }
 done:
  while ((n_pending_returns > 0) && (scan_output < end_output))
    {
      (*scan_output++) = '\r';
      n_pending_returns -= 1;
    }
  (*scan_output) = '\0';
  return (APR_SUCCESS);
}

/* Request handler */

#define CVT_ERROR(expr, msg) do						\
{									\
  apr_status_t CVT_ERROR_status = (expr);				\
  if (APR_SUCCESS != CVT_ERROR_status)			\
    {									\
      ap_log_error (APLOG_MARK, APLOG_ERR, CVT_ERROR_status,		\
						    (r->server), "error %s", msg);			\
      close_lisp_socket (cfg);						\
      return (HTTP_INTERNAL_SERVER_ERROR);				\
    }									\
} while (0)

static int
lisp_handler (request_rec * r)
{
  lisp_cfg_t * cfg
    = (ap_get_module_config ((r->per_dir_config), (&lisp_module)));
  int content_length = (-1);
  int keep_socket_p = 0;
  apr_socket_t * socket;
  const char * request_content_length = 0;

  cfg = local_lisp_cfg(cfg);

  if ((strcmp ((r->handler), "lisp-handler")) != 0)
    return (DECLINED);

  /* Open a connection to the Lisp process.  */
  ML_LOG_DEBUG (r, "open lisp connection");
  CVT_ERROR ((open_lisp_socket (cfg)), "opening connection to Lisp");
  (SERVER_SOCKET_SAFE_P (cfg)) = 0;
  socket = (SERVER_SOCKET (cfg));

  /* Remove any timeout that might be left over from earlier.  */
  ML_LOG_DEBUG (r, "clear socket timeout");
  CVT_ERROR ((apr_socket_timeout_set (socket, (-1))), "clearing read timeout");
  
  /* Convert environment variables to headers and send them.  */
  ML_LOG_DEBUG (r, "write env-var headers");
  ap_add_cgi_vars (r);
  ap_add_common_vars (r);
  if ((r->subprocess_env) != 0)
    CVT_ERROR
      ((copy_headers
	 	((r->subprocess_env), map_env_var_to_lisp_header, socket)),
              "writing to Lisp");

  /* Send this before client headers so ASSOC can be used to grab it
     without worrying about some joker sending a server-id header of
     his own.  (Robert Macomber) */
  ML_LOG_DEBUG (r, "write headers");
  CVT_ERROR ((write_lisp_header (socket, "server-id", (cfg->server_id))),
	     	     "writing to Lisp");

  CVT_ERROR ((write_lisp_header (socket, "server-baseversion", AP_SERVER_BASEVERSION)),
	     	     "writing to Lisp");
  CVT_ERROR ((write_lisp_header (socket, "modlisp-version", VERSION_STRING)),
	     	     "writing to Lisp");
  CVT_ERROR ((write_lisp_header (socket, "modlisp-major-version", "2")),
	     	     "writing to Lisp");
  /* Send all the remaining headers.  */
  if ((r->headers_in) != 0)
    CVT_ERROR
      ((copy_headers ((r->headers_in), map_header_to_lisp_header, socket)),
              "writing to Lisp");

  request_content_length = apr_table_get(r->headers_in, "Content-Length");

  /* Send the end-of-headers marker.  */
  ML_LOG_DEBUG (r, "write end-of-headers");
  CVT_ERROR ((write_lisp_line (socket, "end")), "writing to Lisp");

  /* Send the request entity.  */
  RELAY_HTTP_ERROR (ap_setup_client_block (r, REQUEST_CHUNKED_DECHUNK));
  if (ap_should_client_block (r))
    {
      char buffer [4096];
      ML_LOG_DEBUG (r, "write entity");
      while (1)
	{
	  long n_read = (ap_get_client_block (r, buffer, (sizeof (buffer))));
	  if (n_read < 0)
	    {
	      ML_LOG_PERROR (r, "error reading from client");
	      close_lisp_socket (cfg);
	      return (HTTP_INTERNAL_SERVER_ERROR);
	    }

	  /* for chunked case, when nread == 0, we will write 
	   * a terminating 0.*/
	  
	  {
	    apr_status_t status = APR_SUCCESS; 
	    
	    /* if there's no Content-Type header, the data must be chunked */
	    if (request_content_length == NULL)
	      status = write_lisp_data_chunk (socket, buffer, n_read);
	    else if (n_read != 0)
	      status = write_lisp_data (socket, buffer, n_read);
	    
	    if (APR_SUCCESS != status)
	      {
		while ((ap_get_client_block (r, buffer, sizeof(buffer)))
		       > 0)
		  ;
		ML_LOG_ERROR (status, r, "writing to Lisp");
		close_lisp_socket (cfg);
		return (HTTP_INTERNAL_SERVER_ERROR);
	      }
	  }
	  if( n_read == 0)
	    break;
	}
    }

  /* Set up read timeout so we don't hang forever if Lisp is wedged.  */
  ML_LOG_DEBUG (r, "set socket timeout");
  CVT_ERROR ((apr_socket_timeout_set (socket, READ_TIMEOUT)),
	     	     "setting read timeout");

  /* Read the headers and process them.  */
  ML_LOG_DEBUG (r, "read headers");
  while (1)
    {
      char header_name [4096];
      char header_value [MAX_STRING_LEN];
      CVT_ERROR
	((read_lisp_line (socket, header_name, (sizeof (header_name)))),
	 	 "reading from Lisp");

      if ((strcasecmp (header_name, "end")) == 0)
	break;

      CVT_ERROR
	((read_lisp_line (socket, header_value, (sizeof (header_value)))),
	 	 "reading from Lisp");

      if ((strcasecmp (header_name, "content-type")) == 0)
	{
	  char * tmp = (apr_pstrdup ((r->pool), header_value));
	  ap_content_type_tolower (tmp);
	  (r->content_type) = tmp;
	}
      else if ((strcasecmp (header_name, "status")) == 0)
	{
	  (r->status) = (atoi (header_value));
	  (r->status_line) = (apr_pstrdup ((r->pool), header_value));
	}
      else if ((strcasecmp (header_name, "location")) == 0)
	apr_table_set ((r->headers_out), header_name, header_value);
      else if ((strcasecmp (header_name, "content-length")) == 0)
	{
	  apr_table_set ((r->headers_out), header_name, header_value);
	  content_length = (atoi (header_value));
	}
      else if ((strcasecmp (header_name, "lisp-content-length")) == 0)
	{
	  content_length = (atoi (header_value));
	}
      else if ((strcasecmp (header_name, "last-modified")) == 0)
	{
	  apr_time_t mtime = (apr_date_parse_http (header_value));
	  r->mtime = mtime;
	  ap_set_last_modified (r);
	}
      else if ((strcasecmp (header_name, "keep-socket")) == 0)
	keep_socket_p = (atoi (header_value));
      else if ((strcasecmp (header_name, "log-emerg")) == 0)
	ap_log_error (APLOG_MARK, APLOG_EMERG, APR_SUCCESS, (r->server),
				  		      "%s", header_value);
      else if ((strcasecmp (header_name, "log-alert")) == 0)
	ap_log_error (APLOG_MARK, APLOG_ALERT, APR_SUCCESS, (r->server),
				  		      "%s", header_value);
      else if ((strcasecmp (header_name, "log-crit")) == 0)
	ap_log_error (APLOG_MARK, APLOG_CRIT, APR_SUCCESS, (r->server),
				  		      "%s", header_value);
      else if ((strcasecmp (header_name, "log-error")) == 0)
	ap_log_error (APLOG_MARK, APLOG_ERR, APR_SUCCESS, (r->server),
				  		      "%s", header_value);
      else if ((strcasecmp (header_name, "log-warning")) == 0)
	ap_log_error (APLOG_MARK, APLOG_WARNING, APR_SUCCESS, (r->server),
				  		      "%s", header_value);
      else if ((strcasecmp (header_name, "log-notice")) == 0)
	ap_log_error (APLOG_MARK, APLOG_NOTICE, APR_SUCCESS, (r->server),
				  		      "%s", header_value);
      else if ((strcasecmp (header_name, "log-info")) == 0)
	ap_log_error (APLOG_MARK, APLOG_INFO, APR_SUCCESS, (r->server),
				  		      "%s", header_value);
      else if ((strcasecmp (header_name, "log-debug")) == 0)
	ap_log_error (APLOG_MARK, APLOG_DEBUG, APR_SUCCESS, (r->server),
				  		      "%s", header_value);
      else if ((strcasecmp (header_name, "log")) == 0)
	ap_log_error (APLOG_MARK, APLOG_ERR, APR_SUCCESS, (r->server),
				  		      "%s", header_value);
      else if ((strcasecmp (header_name, "note")) == 0)
	{
	  char * p = (strchr (header_value, ' '));
	  if (p != 0)
	    {
	      (*p++) = '\0';
	      apr_table_setn ((r->notes),
			      			      (apr_pstrdup ((r->pool), header_value)),
						      			      (apr_pstrdup ((r->pool), p)));
	    }
	}
      else if ((strcasecmp (header_name, "set-cookie")) == 0)
	{
	  apr_table_add ((r->headers_out), header_name, header_value);
	}
      else
	apr_table_set ((r->headers_out), header_name, header_value);
    }

  /* Copy the reply entity from Lisp to the client...  */
  //  if (content_length > 0)
  {
    unsigned int n_read = 0;
    input_buffer_t * buffer;

    ML_LOG_DEBUG (r, "read entity");
    CVT_ERROR ((get_input_buffer (socket, (&buffer))), "reading from Lisp");
    while ((buffer->start) <= (buffer->end))
      {
	apr_status_t fill_status;
        unsigned int n_bytes = ((buffer->end) - (buffer->start));
        n_read += n_bytes;
        if ((content_length >= 0) && (n_read > content_length))
          {
            n_bytes -= (n_read - content_length);
            n_read -= (n_read - content_length);
          }
        /* ...unless it's a HEAD request. */
        if (!r->header_only && !write_client_data (r, (buffer->start), n_bytes))
          {
            close_lisp_socket (cfg);
            return (HTTP_INTERNAL_SERVER_ERROR);
          }
        (buffer->start) += n_bytes;
        if (n_read == content_length)
          break;

        fill_status = fill_input_buffer (socket);
        if ((fill_status == APR_EOF) && (content_length < 0))
          break;
        else
          CVT_ERROR (fill_status, "reading from Lisp");
      }
  }
  if ((content_length < 0) || (!keep_socket_p))
    CVT_ERROR ((close_lisp_socket (cfg)), "closing connection to Lisp");
  else
    (SERVER_SOCKET_SAFE_P (cfg)) = 1;
  ML_LOG_DEBUG (r, "request finished");
  return (OK);
}

static apr_status_t
copy_headers (apr_table_t * table, header_map_t * map_name,
			  	      apr_socket_t * socket)
{
  const apr_array_header_t * h = (apr_table_elts (table));
  const apr_table_entry_t * scan = ((apr_table_entry_t *) (h->elts));
  const apr_table_entry_t * end = (scan + (h->nelts));
  while (scan < end)
    {
      const char * name = ((*map_name) (scan->key));
      if (name != 0)
	RELAY_ERROR (write_lisp_header (socket, name, (scan->val)));
      scan += 1;
    }
  return (APR_SUCCESS);
}

static const char *
map_env_var_to_lisp_header (const char * var)
{
  const char * plist [] = {
    "REQUEST_URI", "url",
    "CONTENT_TYPE", "content-type",
    "CONTENT_LENGTH", "content-length",
    "REQUEST_METHOD", "method",
    "REMOTE_ADDR", "remote-ip-addr",
    "REMOTE_PORT", "remote-ip-port",
    "SERVER_ADDR", "server-ip-addr",
    "SERVER_PORT", "server-ip-port",
    "SERVER_PROTOCOL", "server-protocol",
    "SCRIPT_FILENAME", "script-filename",
    "SSL_SESSION_ID", "ssl-session-id",
    0
    };
  const char ** p = plist;
  if ((var == 0) || ((strncmp (var, "HTTP_", 5)) == 0))
    return (0);
  while (1)
    {
      const char * v = (*p++);
      if (v == 0)
	return (0);
      if ((strcmp (v, var)) == 0)
	return (*p);
      p += 1;
    }
}

static const char *
map_header_to_lisp_header (const char * name)
{
  return
    (((name != 0) && ((strcasecmp (name, "end")) == 0))
          ? "end-header"
	       : name);
}

static int
write_client_data (request_rec * r, const char * data, unsigned int n_bytes)
{
  while (n_bytes > 0)
    {
      int n = (ap_rwrite (data, n_bytes, r));
      if (n < 0)
	{
	  ML_LOG_PERROR (r, "error writing to client");
	  return (0);
	}
      n_bytes -= n;
      data += n;
    }
  return (1);
}

/* Module setup */

static int
lisp_post_config (apr_pool_t * cfg_pool,
			     		  apr_pool_t * log_pool,
					  		  apr_pool_t * temp_pool,
							  		  server_rec * s)
{
  ap_add_version_component (cfg_pool, "mod_lisp2/" VERSION_STRING);
  return (OK);
}

static apr_status_t
destroy_socket_pool (void * dummy)
{
#if APR_HAS_THREADS
  apr_threadkey_private_delete(cfg_key);
#endif
  apr_pool_destroy (socket_pool);
  socket_pool = 0;
  return (APR_SUCCESS);
}

static void
lisp_child_init (apr_pool_t * pool, server_rec * s)
{
  if (APR_SUCCESS == apr_pool_create ((&socket_pool), 0))
    {
      apr_pool_cleanup_register (pool, 0, destroy_socket_pool, destroy_socket_pool);
#if APR_HAS_THREADS
      apr_threadkey_private_create(&cfg_key, NULL, socket_pool);
#endif
    }
}

static const command_rec lisp_command_handlers [] =
{
  AP_INIT_TAKE3
    ("LispServer", lisp_set_server, 0, OR_ALL,
     "The Lisp server name, port and ID string."
     "  Example: LispServer 127.0.0.1 3000 \"apache\""),
  {0}
};

static void
register_hooks (apr_pool_t * pool)
{
  ap_hook_post_config (lisp_post_config, 0, 0, APR_HOOK_MIDDLE);
  ap_hook_child_init (lisp_child_init, 0, 0, APR_HOOK_MIDDLE);
  ap_hook_handler (lisp_handler, 0, 0, APR_HOOK_MIDDLE);
}

module AP_MODULE_DECLARE_DATA lisp_module =
{
  STANDARD20_MODULE_STUFF,
  lisp_create_dir_config,	/* create per-directory config structures */
  lisp_merge_config,		/* merge per-directory config structures */
  lisp_create_server_config,	/* create per-server config structures */
  lisp_merge_config,		/* merge per-server config structures */
  lisp_command_handlers,
  register_hooks
};
