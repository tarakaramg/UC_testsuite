direct a {
in x@bla()
}

direct A {A:a}

functionality F() implements A {
 party P serves A {
  initial state S
  {
   match message with
    othermsg => {fail.}
   end
  }

  state T()
  {
   var p:port; var q:qort;
   match message with
    othermsg => {fail.}
   end
  }

 }
}

