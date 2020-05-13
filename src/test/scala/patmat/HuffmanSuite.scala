package patmat

import org.junit._
import org.junit.Assert.assertEquals

class HuffmanSuite {
  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val chs = List('a', 'b', 'a', 'c', 'b', 'a', 'f', 'g', 'a', 'g', 'c', 'b', 't', 't')
  }


  @Test def `weight of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
    }


  @Test def `chars of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(List('a','b','d'), chars(t2))
    }

  @Test def `times of a list of chars (a)`: Unit =
    new TestTrees {
      val t = times(chs)
      Assert.assertEquals(true, !t.find(pair => pair._1 == 'a' && pair._2 == 4).isEmpty)
    }

  @Test def `times of a list of chars (b)`: Unit =
    new TestTrees {
      val t = times(chs)
      Assert.assertEquals(true, !t.find(pair => pair._1 == 'b' && pair._2 == 3).isEmpty)
    }

  @Test def `times of a list of chars (c)`: Unit =
    new TestTrees {
      val t = times(chs)
      Assert.assertEquals(true, !t.find(pair => pair._1 == 'c' && pair._2 == 2).isEmpty)
    }

  @Test def `string2chars hello world`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))


  @Test def `make ordered leaf list for some frequency table (15pts)`: Unit =
    assertEquals(List(Leaf('e',1), Leaf('t',2), Leaf('x',3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))


  @Test def `combine of some leaf list (15pts)`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e',1), Leaf('t',2), List('e', 't'), 3), Leaf('x',4)), combine(leaflist))
  }


  @Test def `decode and encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }

  @Test def `decode and quick encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
