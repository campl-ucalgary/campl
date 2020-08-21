%include "potato/test.ampl" %include "othertest/test.ampl" %handles : {
  IntTerm = {
    Get;
    Put;
    Close 
  }
  }
%cohandles : {
  Console = {
    Get;
    Put;
    Close 
  }
  }
%destructors : {
  Stream = {
    Cons 10 
  }
  }
%functions : {
  adder (a, b) = {
    load b;
    load a;
    add;
    ret 
  }
  }
%processes : {
  }
%run (| inch1, inch2 => outch1, outch2) : {
  }

