(in-package :tr.gen.core.server.test)

(deftest quoted-printable-reader
    (let ((qptext1 "A\"=3D0A=3D=2043&%##430xF00F=0A=20A=20AAAAAAAAAAAAAAAAABBB"))
      (equal (octets-to-string (quoted-printable? (make-core-stream qptext1)) :utf-8)
	     "A\"=0A= 43&%##430xF00F
 A AAAAAAAAAAAAAAAAABBB")
      )
    t)

(deftest quoted-printable-writer
    (let ((cstream (make-core-stream "")))
      (quoted-printable! cstream "A\"=0A= 43&%##430xF00F
 A AAAAAAAAAAAAAAAAABBB")
      (equal (core-server::stream-data cstream)
	     "A\"=3D0A=3D=2043&%##430xF00F=0A=20A=20AAAAAAAAAAAAAAAAABBB")
      )
    t)