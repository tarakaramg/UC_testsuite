requires KeysExponentsAndPlainTexts.

direct a {
in  x@bla()
out bli()@x
}

direct A {A:a}

functionality F() implements A {

 party P serves A {

  initial state I {
   match message with
    sender@othermsg => {send bli()@sender and transition II(e=e).}
   end
  }
 
  state II(b:bool) {
   match message with
    othermsg => {fail.}
   end
  }
 }
}
