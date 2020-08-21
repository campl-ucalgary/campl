-- Good combination of testing the use of codata and data
-- folds [1,2,3] with (+) and 0
%cohandles : {
    ConsoleInt = { Get ; Put ; Close } ;
}

%constructors : {
    List = { Cons 2 ; Nil 0 } ;
}


%destructors : {
    ListFold = { ConsFold 2 ; NilFold 0 } ;
}

%functions : {
    lnil() = { List.Nil ; ret ; } ;
    lcons(s, ss) = { List.Cons(s, ss) ; ret ; } ;
    listfold(folder, list) = {
        case list of {
            List.Nil : {
                ListFold.NilFold folder ;
                ret ;
            } ;
            List.Cons(s, ss) : {
                acc := call listfold(folder, ss) ;
                ListFold.ConsFold(s, acc) folder ;
                ret ;
            } ;
        } ;
        ret ;
    } ;
}


%run ( | console => intterm1 ) : {
    emptylist := call lnil() ;
    a1 := cInt 1 ;
    onenil := call lcons(a1, emptylist) ;
    a2 := cInt 2 ;
    twoonenil := call lcons(a2, onenil) ;
    a3 := cInt 3 ;
    threetwoonenil := call lcons(a3, twoonenil) ;


    sumfold := 
        rec of {
            ListFold.ConsFold(a, acc) : {
                load a ;
                load acc ;
                add ;
                ret ;
            } ;
            ListFold.NilFold() : {
                cInt 0;
                ret ;
            }
        } ; 
    
    sum := call listfold(sumfold, threetwoonenil) ;

    hput ConsoleInt.Put on console ;
    put sum on console ;
    hput ConsoleInt.Close on console ;
}
