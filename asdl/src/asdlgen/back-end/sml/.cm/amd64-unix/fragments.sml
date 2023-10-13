110.98  amd64  
              @       �      +   h4�@�r���Z�p���4�@�r���Z�p���               n               nC��Jdq�/���G�Aguid-driver/(sources.cm):../front-end/(sources.cm):typechecker/(sources.cm):../../back-end/(sources.cm):sml/(sources.cm):fragments.sml-1695275962.736
  �     	!      @    -      O(*---------- begin pickle-util.in ----------*)
    type instream = @PICKLER@.instream
    type outstream = @PICKLER@.outstream
  (* write an option *)
    fun writeOption wrFn (outS, NONE) = @PICKLER@.writeTag8(outS, 0w0)
      | writeOption wrFn (outS, SOME obj) = (
          @PICKLER@.writeTag8(outS, 0w1);
          wrFn(outS, obj))
  (* read an option *)
    fun readOption rdFn inS = (case @PICKLER@.readTag8 inS
           of 0w0 => NONE
            | 0w1 => let val obj = rdFn inS in SOME obj end
            | _ => raise ASDL.DecodeError
          (* end case *))
  (* write a list of values as a sequence *)
    fun writeSeq wrFn (outS, xs) = let
          fun write [] = ()
            | write (x::xr) = (wrFn(outS, x); write xr)
          in
            @PICKLER@.writeUInt(outS, Word.fromInt(length xs));
            write xs
          end
  (* read a sequence into a sequence of values *)
    fun readSeq rdFn inS = let
          val len = @PICKLER@.readUInt inS
          fun read (0w0, xs) = reverse (xs, [])
            | read (n, xs) = let
                val x = rdFn inS
                in
                  read (n-0w1, x::xs)
                end
          and reverse ([], xs) = xs
            | reverse (x::xr, xs) = reverse (xr, x::xs)
          in
            read (len, [])
          end
(*---------- end pickle-util.in ----------*)
-      �(*---------- begin sexp-util.in ----------*)
    structure SExpPkl = @PICKLER@
    type instream = SExpPkl.instream
    type outstream = SExpPkl.outstream
    val writeSExp = SExpPkl.writeSExp
    val writeOption = SExpPkl.writeOption
    val readOption = SExpPkl.readOption
    val writeSeq = SExpPkl.writeSeq
    val readSeq = SExpPkl.readSeq
    val writeEnum = SExpPkl.writeEnum
    val space = SExpPkl.space
(*---------- end sexp-util.in ----------*)
,�(*---------- begin streams.in ----------*)
    type instream = @PICKLER@.instream
    type outstream = @PICKLER@.outstream
(*---------- end streams.in ----------*)
200�   8    I;�w	H�E H�(��� �T$@A���back-end/sml/fragments.sml     1p�Fragments"5DCA nff3pa"pickleUtil"4��nC"string"0 pa"sexpUtil"4�&pa"streams"4�&00sABi1�A 3�B�� �� 