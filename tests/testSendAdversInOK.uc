requires KeysExponentsAndPlainTexts.

direct d {
in  x@bla()
out bli()@x
}

direct D {D:d}

adversarial a {
in  bla()
out bli()
}

adversarial A {A:a}

functionality F() implements D A {

 party P serves A, D {

  initial state I {
   match message with
     sender@D.othermsg => {send A.bli() and transition I.}
   | othermsg => {fail.}
   end
  }
 }
}
