direct d {
in x@bla()
out bli()@x
}

direct D {D:d}

functionality F(P:D) implements D {

party Y serves D.D {
  initial state Is 
  {
   match message with
     D.bli => {fail.}
   | * => {fail.}
   end
  }
}
 
}

