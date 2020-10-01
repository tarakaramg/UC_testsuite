ec_requires KeysExponentsAndPlainTexts.

direct a {
in  x@bla()
out bli()@x
}

direct A {A:a}

functionality F() implements A {

 party P serves A.A {

  initial state I {
   match message with
    sender@A.A.bla() => {send A.A.bli()@sender and transition II(gen e e).}
   end
  }
 
  state II(k:key) {
   match message with
    * => {fail.}
   end
  }
 }
}
