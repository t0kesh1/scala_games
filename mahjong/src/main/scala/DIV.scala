object DIV{
	type Parser = String => Option[(Char,String)]
	def parser(s:String):Parser = {s => if (s.isEmpty) None else Some(s.head,s.tail)}

	def unapply[A](t:(List[A],List[A])): Option[(List[A],List[A])] = t match {
		case (x,y) if y.diff(x).isEmpty => Some(x.diff(y),y)
		case _ => None
	}
}
