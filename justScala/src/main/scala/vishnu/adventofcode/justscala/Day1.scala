package vishnu.adventofcode.justscala

object Day1 {
  def findPair(l: List[Int], target: Int) = {
    val mSet = scala.collection.mutable.Set(l: _*)
    val rel = l.find { i =>
      mSet -= i
      mSet.contains(target - i)
    }
    rel.map(r => r * (target - r)).get
  }

  def findTriples(l: List[Int], target: Int) = {
    val mSet = scala.collection.mutable.Set(l: _*)

    val rel = (for {
      e1 <- l
      e2 <- l.tail
    } yield {
      (e1, e2)
    }).find { case (i, j) =>
      mSet -= i
      mSet -= j
      val b = mSet.contains(target - (i + j))
      mSet += i
      mSet += j
      b
    }
    rel.map { case (i, j) => i * j * (target
            - (i + j))
    }.get
  }

  def main(args: Array[String]): Unit = {
        val l = List(1721,
          979,
          366,
          299,
          675,
          1456)
        println(findPair(l, 2020))
//    val l = List(1728, 1621, 1856, 1683, 1940, 1097, 1711, 1906, 2008, 1608, 2003, 1990, 1864, 1035, 1981, 1978, 1955, 1907, 1198, 1087, 1835, 1961, 1941, 1903, 1675, 417, 1842, 1802, 1639, 1601, 1546, 1909, 1061, 1031, 1996, 1717, 1972, 1900, 1443, 1873, 1851, 2010, 1650, 1975, 1002, 1142, 1747, 1640, 1924, 1824, 1539, 1937, 1715, 1871, 1867, 1428, 1861, 1914, 1986, 1976, 1111, 1858, 1869, 1899, 1171, 1041, 1662, 1222, 1709, 1889, 1950, 1960, 1989, 1737, 1600, 1444, 1725, 1710, 1653, 1745, 1922, 1945, 1189, 1917, 1891, 1718, 1997, 1631, 1053, 1750, 1634, 1822, 1706, 1160, 1619, 1665, 1687, 1648, 1818, 1655, 1736, 1881, 489, 1598, 1923, 1962, 1918, 1689, 1616, 1825, 1723, 1767, 591, 1734, 1949, 1645, 1344, 1959, 1758, 1068, 1843, 1826, 1849, 2005, 1777, 144, 2009, 1982, 1911, 1288, 1595, 1094, 2000, 1713, 1973, 1971, 1916, 1666, 1105, 1806, 1868, 1944, 1654, 1809, 1726, 1672, 1060, 1065, 1521, 1921, 1966, 1113, 1149, 1607, 1980, 1023, 1855, 1948, 1638, 1930, 1866, 1954, 1697, 1884, 1832, 2004, 914, 1845, 1043, 1854, 1223, 1913, 1984, 1910, 1793, 1878, 1248, 617, 1927, 1527, 1819, 1350, 1807, 1722, 1016, 1700, 111, 1629, 1932, 1644, 1454, 1987, 1925, 1953, 1743, 1180, 1782, 1523, 1245, 1620)
//    //    println(findPair(l, 2020))
//    println(findTriples(l, 2020))

  }
}
