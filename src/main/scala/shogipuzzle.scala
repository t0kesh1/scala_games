class shogipuzzle{
  def move(board:(List[Symbol],List[Symbol])): List[(List[Symbol],List[Symbol])] = board match{
    case (Nil,Nil) => List((Nil,Nil))
    case _ => {
      val uemigi:(List[Symbol],List[Symbol]) = board match {
        case ((kgr :: 'v :: rest), lower) if(kgr == 'k||kgr == 'g||kgr == 'r) => (('v::kgr::rest),lower)
        case _ => board
      }
      val uemigimigi:(List[Symbol],List[Symbol]) = board match {
        case (('r :: 'v :: 'v :: rest), lower) => (('v::'v::'r::rest),lower)
        case _ => board
      }
      val uehidari:(List[Symbol],List[Symbol]) = board match {
        case (('v :: kgr :: rest), lower) if(kgr == 'k||kgr == 'g||kgr == 'r) => ((kgr::'v::rest),lower)
        case _ => board
      }
      val uehidarihidari:(List[Symbol],List[Symbol]) = board match {
        case (('v :: 'v :: 'r :: rest), lower) => (('r::'v::'v::rest),lower)
        case _ => board
      }
      val shitamigi:(List[Symbol],List[Symbol]) = board match {
        case (upper, (kgr :: 'v :: rest)) if(kgr == 'k||kgr == 'g||kgr == 'r) => (upper,('v::kgr::rest))
        case _ => board
      }
      val shitamigimigi:(List[Symbol],List[Symbol]) = board match {
        case (upper, ('r :: 'v :: 'v :: rest)) => (upper, ('v::'v::'r::rest))
        case _ => board
      }
      val shitahidari:(List[Symbol],List[Symbol]) = board match {
        case (upper, ('v :: kgr :: rest)) if(kgr == 'k||kgr == 'g||kgr == 'r) => (upper, (kgr::'v::rest))
        case _ => board
      }
      val shitahidarihidari:(List[Symbol],List[Symbol]) = board match {
        case (upper, ('v :: 'v :: 'r :: rest)) => (upper, ('r::'v::'v::rest))
        case _ => board
      }
      val sagaru:(List[Symbol],List[Symbol]) = board match {
        case ((kgr :: rest), ('v :: rest2)) if(kgr == 'k||kgr == 'g||kgr == 'r) => (('v :: rest), (kgr :: rest2))
        case _ => board
      }
      val sagarumigi:(List[Symbol],List[Symbol]) = board match {
        case ((kbs :: rest), (x :: 'v :: rest2)) if(kbs == 'k||kbs == 'b||kbs == 's) => (('v :: rest),(x :: kbs :: rest2))
        case _ => board
      }
      val sagaruhidari:(List[Symbol],List[Symbol]) = board match {
        case ((x :: kbs :: rest), ('v  :: rest2)) if(kbs == 'k||kbs == 'b||kbs == 's) => ((x :: 'v :: rest), (kbs  :: rest2))
        case _ => board
      }
      val agaru:(List[Symbol],List[Symbol]) = board match {
        case (('v :: rest), (kgsr :: rest2)) if(kgsr == 'k||kgsr == 'g||kgsr == 's||kgsr == 'r) => ((kgsr :: rest), ('v :: rest2))
        case _ => board
      }
      val agarumigi:(List[Symbol],List[Symbol]) = board match {
        case ((x :: 'v :: rest), (kbgs  :: rest2)) if(kbgs == 'k||kbgs == 'b||kbgs == 'g||kbgs == 's) => ((x :: kbgs :: rest), ('v  :: rest2))
        case _ => board
      }
      val agaruhidari:(List[Symbol],List[Symbol]) = board match {
        case (('v :: rest),(x :: kbgs :: rest2)) if(kbgs == 'k||kbgs == 'b||kbgs == 'g||kbgs == 's) => ((kbgs :: rest), (x :: 'v :: rest2))
        case _ => board
      }
      val rec:List[(List[Symbol],List[Symbol])] = move((board._1.tail,board._2.tail)).map{case (a:List[Symbol],b:List[Symbol]) => ((board._1.head)::a,(board._2.head) :: b)}
      (List(uehidari,uemigi,shitamigi,shitahidari,sagaru,sagarumigi,sagaruhidari,agaru,agarumigi,agaruhidari):::rec)
      .distinct
    }
  }
}