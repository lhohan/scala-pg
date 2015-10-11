package misc

/**
 * Created by hans on 25/01/15.
 */
object SudokuSolver {

  // TODO : only stream in case of infinite lists ...?
  // TODO: consistent method signature: functional args as it is more in spirit of book and functional style
  // TODO: check out compose

  type Row[A] = Stream[A]
  type Matrix[A] = Stream[Row[A]]
  type Digit = Int
  // Book: Char, keep it more simple, less typing during tests
  type Grid = Matrix[Digit]
  val digits = (1 to 9).toStream

  val Blank = 0
  def blank = (x: Int) => x == Blank

  type Choices = Stream[Digit] // TODO Row[Digit]

  // given a grid, create matrix where each field contains a Stream of possible choices
  def choices(g: Grid): Matrix[Choices] = {
    def choice(d: Digit): Choices = if (blank(d)) digits else Stream(d)
    g.map(_.map(choice)) // TODO: can be more concise?
  }

  // given the result of `choices` expand it to a Stream of Grids with all possible combinations
  def expand(mc: Matrix[Choices]): Stream[Grid] = {
    val possibleChoicesForEachRow = mc.map(cartesianProduct)
    cartesianProduct(possibleChoicesForEachRow) // think about it, write it down to understand
  }

  def cartesianProduct[A](m: Stream[Stream[A]]): Stream[Stream[A]] = m match {
    case Stream() => Stream(Stream.empty[A])
    case xs #:: xss => for {
      x <- xs
      ys <- cartesianProduct(xss)
    } yield x #:: ys
  }


  // filter to find out the valid grids = no duplicates in rows, cols and boxes
  def valid(g: Grid): Boolean =
    cols(g).forall(noDups) &&
    rows(g).forall(noDups) &&
    boxs(g).forall(noDups)

  def noDups[A]: Stream[A] => Boolean = {
    case Stream() => true
    case x #::xs => xs.forall(x != _) && noDups(xs)
  }

  def rows[A]: (Matrix[A]) => Matrix[A] = m => m

  def cols[A]: (Matrix[A]) => Matrix[A] = {
    // matrix transposition
    case Stream(xs) => xs.map(Stream(_))
    case xs #:: xss => xs.zip(cols(xss)).map{case (x,y) => x #:: y}
  }

  def printable[A](m:Matrix[A]):List[List[A]] = m.map(_.toList).toList

  def boxs[A]: (Matrix[A]) => Matrix[A] = m => {
    // draw the sequence to understand

    // we 'collapse' (group) the parts in the matrix to the units we want to move around
    val grouped = group(m.map(group))
    // then we move to get our parts in their place, transpose each grouped part
    val transposed = grouped.map(cols)
    // everything is place so: flatten everything again
    val ungrouped = ungroup(transposed).map(ungroup)
    ungrouped
  }

  // split list in groups of 3
  def group[A]: Stream[A] => Stream[Stream[A]] =  {
    case Stream() => Stream.empty[Stream[A]]
    case xs => xs.take(3) #:: group(xs.drop(3))
  }

  def ungroup[A]: Stream[Stream[A]] => Stream[A] = xss => for {
    xs <- xss
    x <- xs
  } yield x

  // nice expression of how we are solving, too slow tough
  def solve(g: Grid): Stream[Grid] = expand(choices(g)) filter valid

  def prune: Matrix[Choices] => Matrix[Choices] = m =>
    pruneBy(cols)(pruneBy(rows)(pruneBy(boxs)(m)))

  def pruneRow: Row[Choices] => Row[Choices] = row => {
    def singleton: Choices => Boolean = c => c.size == 1
    // list of all singletons
    def fixed: Choices = (for {
      c <- row if singleton(c)
    } yield c).map(_.head) // TODO: better way
    // remove all digits in singleton from the other choices
    def remove: (Choices, Choices) => Choices = (xs, ds) =>
        if(singleton(xs)) xs else xs.filterNot(x => ds.contains(x))
    row.map(c => remove(c, fixed))
  }

  def pruneBy(f: (Matrix[Choices]) => Matrix[Choices])(m : Matrix[Choices]) = f(f(m).map(pruneRow))

  // with pruning
  def solve2(g: Grid): Stream[Grid] = expand(prune(choices(g))) filter valid

  // repeatedly pruning
  def solve2_(g: Grid): Stream[Grid] = {
    def nextPrune(matrix: Matrix[Choices]) = (prune(matrix), matrix)
    def loop(choices: Matrix[Choices]): Matrix[Choices] = {
      nextPrune(choices) match {
        case (pruned, orig) if pruned == orig => pruned
        case (pruned,_) => loop(prune(pruned))
      }
    }
    val fullyPruned = loop(choices(g))
    expand(fullyPruned) filter valid
  }


  // solution 3
  def counts(rows: Matrix[Choices]): Stream[Int] = rows.flatten.map(_.length).filter(_!=1)

  def expand1(rows: Matrix[Choices]) = {
    val n = counts(rows).min

   ???
  }






}
