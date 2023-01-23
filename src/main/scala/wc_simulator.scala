import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object wc_simulator extends App{


  //var pkt = scala.collection.mutable.Seq.empty[Int,(Int,Int,Int,Int)]
  private val r1_16_tmp: mutable.Builder[String, Seq[String]] = Seq.newBuilder[String]

  private val r = scala.util.Random

  case class t(o: Int, p: Int, a: Int) {

    def min: Int = {
      math.min(o, math.min(p, a))
    }

    def max: Int = {
      math.max(o, math.max(p, a))
    }

  }

  private def check_m(m: ArrayBuffer[Int], id_1: Int, id_2: Int): Int = {

    if(m.length == 6)
      if(id_1 == 0 && id_2 == 1) m(0)
      else if(id_1 == 0 && id_2 == 2) m(1)
      else if(id_1 == 0 && id_2 == 3) m(2)
      else if(id_1 == 1 && id_2 == 2) m(3)
      else if(id_1 == 1 && id_2 == 3) m(4)
      else if(id_1 == 2 && id_2 == 3) m(5)
      else m(999)
    else if(m.length == 3)
      if(id_1 == 0 && id_2 == 1) m(0)
      else if(id_1 == 0 && id_2 == 2) m(1)
      else if(id_1 == 1 && id_2 == 2) m(2)
      else m(999)
    else m(999)

  }

  private def fill(p: t, max: Int, min: Int, minutes: Int) = {

    val A = List.fill(minutes)(
      {
        val par = min

        val n = r.nextInt(3 * max + par)

        val res = if (n < par + p.o) "O"
        else if (n < par + p.o + p.p) "R"
        else if (n < par + p.o + p.p + p.a) "A"
        else " "

        res

      }

    )
    A

  }

  private def result(t1: t, t2: t, minutes: Int): (Int, Int) = {

    val max = math.max(t1.max, t2.max)
    val min = math.min(t1.min, t2.min)

    val tA: List[String] = fill(t1, max, min, minutes)
    val tB: List[String] = fill(t2, max, min, minutes)

    val (g_a, g_b): (Int, Int) = result(tA, tB): (Int, Int)
    (g_a, g_b)

  }

  private def result(a: List[String], b: List[String]): (Int, Int) = {

    var act = "x"
    var pm_a = 0
    var pm_b = 0
    var g_a = 0
    var g_b = 0

    for (i <- a.indices) {
      //println(i"$act${a(i)}${b(i)}")

      act match {
        case "A" =>
          if (a(i) != "R") pm_a = 0
          if (a(i) == "A" && b(i) == "O") {
            act = "B"; /*println("K!")*/ ;
            pm_a = 0
            pm_b = 0
          }
          else if (a(i) == "A" && b(i) != "O") {
            act = "x"
            g_a += 1
            //println(i"GA:$i")
          }
          else act = "x"
        case "B" =>
          if (b(i) != "R") pm_b = 0
          if (b(i) == "A" && a(i) == "O") {
            act = "A"; /*println("K!");*/ pm_a = 0
            pm_b = 0
          }
          else if (b(i) == "A" && a(i) != "O") {
            act = "x"
            g_b += 1
            //println(i"GB:$i")
          }
          else act = "x"

        case _ =>

      }

      if (act == "x") {
        if (a(i) == "R") pm_a += 1
        else pm_a = 0
        if (b(i) == "R") pm_b += 1
        else pm_b = 0

        if (pm_a > pm_b && b(i) != "O") act = "A"
        else if (pm_a < pm_b && a(i) != "O") act = "B"
      }


    }

    //println(s"$g_a:$g_b")

    val res = (g_a, g_b)

    res
  }

  //play single match nr times
  def result(t_1: t, t_2: t, minutes: Int, nr: Int): (Int, Int) = {

    val pkt = scala.collection.mutable.ArrayBuffer.empty[Int]
    val gs = scala.collection.mutable.ArrayBuffer.empty[Int]
    val gc = scala.collection.mutable.ArrayBuffer.empty[Int]


    val max = math.max(t_1.max, t_2.max)
    val min = math.min(t_1.min, t_2.min)

    for (_ <-0 until nr) {

      val t_A: List[String] = fill(t_1, max, min, minutes)
      val t_B: List[String] = fill(t_2, max, min, minutes)

      val (g_a, g_b): (Int, Int) = result(t_A: List[String], t_B: List[String]): (Int, Int)
      pkt += g_a - g_b
      gs += g_a
      gc += g_b

    }

    val pkt_s = pkt.zipWithIndex.sorted

    //println(pkt_s)
    //val median = pkt_s((nr - 1) / 2)

    val m_gs = gs(pkt_s((nr - 1) / 2)._2)
    val m_gc = gc(pkt_s((nr - 1) / 2)._2)

    //println(s"${gs.sum}:${gc.sum}")

    (m_gs, m_gc)
  }

  private def extra_time(t1: t, t2: t, minutes: Int = 30): (Int, Int) = result(t1, t2, minutes)
  private def regular_time(t1: t, t2: t, minutes: Int = 90): (Int, Int) = result(t1, t2, minutes)

  private def play_off(t1: (String, t), t2: (String, t)): (String, t) = {

    val match_result: (Int, Int) = regular_time(t1._2, t2._2)

    val advanced_team_stats: (String, t, (Int, Int), String) =
      if (match_result._1 > match_result._2) (t1._1, t1._2, (0, 0), "regular time")
      else if (match_result._1 < match_result._2) (t2._1, t2._2, (0, 0), "regular time")
      else /*extra-time*/ {
        val et: (Int, Int) = extra_time(t1._2, t2._2)
        if (et._1 > et._2) (t1._1, t1._2, et, "after extra time")
        else if (et._1 < et._2) (t2._1, t2._2, et, "after extra time")
        else /*penalty shots*/ {
          val n = r.nextFloat()
          if (n < 0.5) (t1._1, t1._2, et, "penalty shoot-out")
          else (t2._1, t2._2, et, "penalty shootout")
        }
      }

    val result_after_extra_time = (match_result._1 + advanced_team_stats._3._1, match_result._2 + advanced_team_stats._3._2)

    println(s"${t1._1}-${t2._1} $match_result -> ${if(advanced_team_stats._4.eq("regular time")) advanced_team_stats._4 else result_after_extra_time.toString() ++ " " ++ advanced_team_stats._4} -> ${advanced_team_stats._1} ")
    (advanced_team_stats._1, advanced_team_stats._2)
  }


  private def pickWinner(table: ArrayBuffer[Int], goalDif: ArrayBuffer[Int], matches: ArrayBuffer[Int]): Int = {

    //println(s"matches: $matches")

    val poI = table.zipWithIndex

    //println(s"${table.max}, ${table.count(_ == table.max)}")

    val winner: Int = table.count(_ == table.max) match {
      case 1 => poI.max._2
      case 2 =>

        val ind2 = poI.filter(_._1 == table.max).map(_._2)
        //println(s"ind2 $ind2")

        val I2 = if (goalDif(2 * ind2(0)) - goalDif(2 * ind2(0) + 1) > goalDif(2 * ind2(1)) - goalDif(2 * ind2(1) + 1)) ind2(0)
        else if (goalDif(2 * ind2(0)) - goalDif(2 * ind2(0) + 1) < goalDif(2 * ind2(1)) - goalDif(2 * ind2(1) + 1)) ind2(1)
        else if (goalDif(2 * ind2(0)) > goalDif(2 * ind2(1))) ind2(0)//scored goals
        else if (goalDif(2 * ind2(0)) < goalDif(2 * ind2(1))) ind2(1)
        //direct match
        else if(check_m(matches, ind2(0), ind2(1)) == 3) ind2(0)
        else if(check_m(matches, ind2(0), ind2(1)) == 0) ind2(1)
        else //random promotion
          if(r.nextFloat() < 0.5) ind2(0)
          else ind2(1)
        I2

      case 3 =>

        val ind3 = poI.filter(_._1 == table.max).map(_._2)

        //println(s"ind3 ppp,$ind3")

        val gd1 = goalDif(2 * ind3(0)) - goalDif(2 * ind3(0) + 1)
        val gd2 = goalDif(2 * ind3(1)) - goalDif(2 * ind3(1) + 1)
        val gd3 = goalDif(2 * ind3(2)) - goalDif(2 * ind3(2) + 1)

        //println(s"goal differences,$gd1,$gd2,$gd3")

        val I3 = List(gd1, gd2, gd3).count(_ == math.max(gd1, math.max(gd2, gd3))) match {

          case 1 =>

            //println(s"Leader in goal balance")

            val ind31 = math.max(gd1, math.max(gd2, gd3)) match {

              case `gd1` => ind3(0)
              case `gd2` => ind3(1)
              case `gd3` => ind3(2)
              case _ => 6
            }

            //println(ind31)
            ind31

          case 2 =>

            //println(s"two teams have the same goal balance -> goals scored will decide")

            val ind32: Seq[Int] = List(gd1, gd2, gd3).zip(List(ind3(0), ind3(1), ind3(2))).filter(_._1 == math.max(gd1, math.max(gd2, gd3))).map(_._2)

            //println(s"${ind32(0)},${ind32(1)}")

            if (goalDif(2 * ind32.head) > goalDif(2 * ind32(1))) ind32.head
            else if (goalDif(2 * ind32.head) < goalDif(2 * ind32(1))) ind32(1)
            //direct match
            else if(check_m(matches, ind32.head, ind32(1)) == 3) ind32.head
            else if(check_m(matches, ind32.head, ind32(1)) == 0) ind32(1)
            else //random promotion (equivalent of fair play classification)
              if(r.nextFloat() < 0.5) ind32.head
              else ind32(1)

          //three team have the same gaol balance -> goals scored will decide
          case 3 =>

            //println(s"three team have the same gaol balance -> goals scored will decide")

            val gf1 = goalDif(2 * ind3(0))
            val gf2 = goalDif(2 * ind3(1))
            val gf3 = goalDif(2 * ind3(2))

            //println(s"$gf1,$gf2, $gf3")

            val I33: Int = List(gf1, gf2, gf3).count(_ == math.max(gf1, math.max(gf2, gf3))) match {

              case 1 =>
                math.max(gf1, math.max(gf2, gf3)) match {

                  case `gf1` => ind3(0)
                  case `gf2` => ind3(1)
                  case `gf3` => ind3(2)
                  case _ => 6
                }


              case 2 => //direct match

                //println(s"direct match")

                val ind332: Seq[Int] = List(gf1, gf2, gf3).zipWithIndex.filter(_._1 == math.max(gf1, math.max(gf2, gf3))).map(_._2)

                //println(s"${check_m(matches, ind332(0), ind332(1))}")

                val index = if(check_m(matches, ind332.head, ind332(1)) == 3) ind332.head
                else if(check_m(matches, ind332.head, ind332(1)) == 0) ind332(1)
                else //random promotion
                  if(r.nextFloat() < 0.5) ind332.head
                  else ind332(1)

                ind3(index)


              case 3 => //same points, goal balance and goals scored for three teams; check points in direct matches

                //println(s"same points, goal balance and goals scored for three teams; check points in direct matches")

                val ind33_0_points: Int =
                  check_m(matches, ind3(0), ind3(1)) + check_m(matches, ind3(0), ind3(2))
                val ind33_1_points: Int =
                  if(check_m(matches, ind3(0), ind3(1)) == 1) 1 else 3 - check_m(matches, ind3(0), ind3(1)) + check_m(matches, ind3(1), ind3(2))
                val ind33_2_points: Int =
                  (if(check_m(matches, ind3(0), ind3(2)) == 1) 1 else 3 - check_m(matches, ind3(0), ind3(2))) +
                    (if(check_m(matches, ind3(1), ind3(2)) == 1) 1 else 3 - check_m(matches, ind3(1), ind3(2)))

                /*println(s"check, " +
                  s"${check_m(matches, ind3(0), ind3(1)) + check_m(matches, ind3(0), ind3(2))}" +
                  s"${if(check_m(matches, ind3(0), ind3(1)) == 1) 1 else 3 - check_m(matches, ind3(0), ind3(1)) + check_m(matches, ind3(1), ind3(2))}" +
                  s"${(if(check_m(matches, ind3(0), ind3(2)) == 1) 1 else 3 - check_m(matches, ind3(0), ind3(2))) +
                    (if(check_m(matches, ind3(1), ind3(2)) == 1) 1 else 3 - check_m(matches, ind3(1), ind3(2)))}"
                )*/
                //println(s"points,$ind33_0_points,$ind33_1_points,$ind33_2_points")

                val ind333 = List(ind33_0_points, ind33_1_points, ind33_2_points).iterator.count(p => p == math.max(ind33_0_points, math.max(ind33_1_points, ind33_2_points))) match {

                  case 1 =>

                    val ind3331: Int = math.max(ind33_0_points, math.max(ind33_1_points, ind33_2_points)) match {

                      case `ind33_0_points` => ind3(0)
                      case `ind33_1_points` => ind3(1)
                      case `ind33_2_points` => ind3(2)
                      case _ => 6
                    }
                    ind3331

                  case 2 =>

                    val ind3332 = List(ind33_0_points, ind33_1_points, ind33_2_points).zipWithIndex.filter(_._1 == math.max(ind33_0_points, math.max(ind33_1_points, ind33_2_points))).map(_._2)

                    val index = if(check_m(matches, ind3332.head, ind3332(1)) == 3) ind3332.head
                    else if(check_m(matches, ind3332.head, ind3332(1)) == 0) ind3332(1)
                    else //random promotion
                      if(r.nextFloat() < 0.5) ind3332.head
                      else ind3332(1)

                    ind3(index)

                  case 3 => //random promotion

                    val rn = r.nextFloat()

                    if(rn < 0.333) ind3(0)
                    else if(rn < 0.666) ind3(1)
                    else ind3(2)

                }

                ind333

            }

            I33

        }

        I3

      case 4 =>

        val ind4 = poI.filter(_._1 == table.max).map(_._2)

        //goal difference filter
        val gd1 = goalDif(0) - goalDif(1)
        val gd2 = goalDif(2) - goalDif(3)
        val gd3 = goalDif(4) - goalDif(5)
        val gd4 = goalDif(6) - goalDif(7)

        val I4 = List(gd1, gd2, gd3, gd4).count(_ == math.max(gd1, math.max(gd2, math.max(gd3, gd4)))) match {

          case 1 =>

            //println(s"goal difference filter - 1")

            val ind41: Int =  math.max(gd1, math.max(gd2, math.max(gd3, gd4))) match {

              case `gd1` => ind4(0)
              case `gd2` => ind4(1)
              case `gd3` => ind4(2)
              case `gd4` => ind4(3)
              case _ => 6
            }
            ind41

          case 2 =>

            //println(s"goal difference filter - 2")

            //more scored
            val ind42 = List(gd1, gd2, gd3, gd4).zipWithIndex.filter(_._1 == math.max(gd1,math.max(gd2, math.max(gd3, gd4)))).map(_._2)

            if (goalDif(2 * ind42.head) > goalDif(2 * ind42(1))) ind4(ind42.head)
            else if (goalDif(2 * ind42.head) < goalDif(2 * ind42(1))) ind4(ind42(1))
            //direct match
            else if(check_m(matches, ind42.head, ind42(1)) == 3) ind4(ind42.head)
            else if(check_m(matches, ind42.head, ind42(1)) == 0) ind4(ind42(1))
            else //random promotion
              if(r.nextFloat() < 0.5) ind4(ind42.head)
              else ind4(ind42(1))

          case 3 =>

            //println(s"goal difference filter - 3")

            //more scored
            val ind43 = List(gd1, gd2, gd3, gd4).zipWithIndex.filter(_._1 == math.max(gd1,math.max(gd2, math.max(gd3, gd4)))).map(_._2)

            val gf1 = goalDif(2 * ind43.head)
            val gf2 = goalDif(2 * ind43(1))
            val gf3 = goalDif(2 * ind43(2))

            //println(s"$gf1,$gf2, $gf3")

            val I43: Int = List(gf1, gf2, gf3).count(_ == math.max(gf1, math.max(gf2, gf3))) match {

              case 1 =>

                math.max(gf1, math.max(gf2, gf3)) match {

                  case `gf1` => ind43.head
                  case `gf2` => ind43(1)
                  case `gf3` => ind43(2)
                  case _ => 6
                }

              case 2 =>

                //println(s"432 - direct match")

                val ind432: Seq[Int] = List(gf1, gf2, gf3).zip(ind43).filter(_._1 == math.max(gf1, math.max(gf2, gf3))).map(_._2)

                //println(s"${check_m(matches, ind432(0), ind432(1))}")

                val index = if(check_m(matches, ind432.head, ind432(1)) == 3) ind432.head
                else if(check_m(matches, ind432.head, ind432(1)) == 0) ind432(1)
                else //random promotion
                  if(r.nextFloat() < 0.5) ind432.head
                  else ind432(1)

                index

              case 3 =>

                //println(s"same points, goal balance and goals scored for three teams; check points in direct matches; reduced table")

                val ind43_0_points: Int =
                  check_m(matches, ind43.head, ind43(1)) + check_m(matches, ind43.head, ind43(2))
                val ind43_1_points: Int =
                  if(check_m(matches, ind43.head, ind43(1)) == 1) 1 else 3 - check_m(matches, ind43.head, ind43(1)) + check_m(matches, ind43(1), ind43(2))
                val ind43_2_points: Int =
                  (if(check_m(matches, ind43.head, ind43(2)) == 1) 1 else 3 - check_m(matches, ind43.head, ind43(2))) +
                    (if(check_m(matches, ind43(1), ind43(2)) == 1) 1 else 3 - check_m(matches, ind43(1), ind43(2)))

                /*println(s"check, " +
                  s"${check_m(matches, ind43(0), ind43(1)) + check_m(matches, ind43(0), ind43(2))}" +
                  s"${if(check_m(matches, ind43(0), ind43(1)) == 1) 1 else 3 - check_m(matches, ind43(0), ind43(1)) + check_m(matches, ind43(1), ind43(2))}" +
                  s"${(if(check_m(matches, ind43(0), ind43(2)) == 1) 1 else 3 - check_m(matches, ind43(0), ind43(2))) +
                    (if(check_m(matches, ind43(1), ind43(2)) == 1) 1 else 3 - check_m(matches, ind43(1), ind43(2)))}"
                )*/
                //println(s"points $ind43_0_points,$ind43_1_points,$ind43_2_points")

                val ind433 = List(ind43_0_points, ind43_1_points, ind43_2_points).iterator.count(p => p == math.max(ind43_0_points, math.max(ind43_1_points, ind43_2_points))) match {

                  case 1 =>

                    val ind4331: Int = math.max(ind43_0_points, math.max(ind43_1_points, ind43_2_points)) match {

                      case `ind43_0_points` => ind43.head
                      case `ind43_1_points` => ind43(1)
                      case `ind43_2_points` => ind43(2)
                      case _ => 6
                    }
                    ind4331

                  case 2 =>

                    val ind4332 = List(ind43_0_points, ind43_1_points, ind43_2_points).zipWithIndex.filter(_._1 == math.max(ind43_0_points, math.max(ind43_1_points, ind43_2_points))).map(_._2)

                    val index = if(check_m(matches, ind4332.head, ind4332(1)) == 3) ind4332.head
                    else if(check_m(matches, ind4332.head, ind4332(1)) == 0) ind4332(1)
                    else //random promotion
                      if(r.nextFloat() < 0.5) ind4332.head
                      else ind4332(1)

                    ind43(index)

                  case 3 => //random promotion

                    val rn = r.nextFloat()

                    if(rn < 0.333) ind43.head
                    else if(rn < 0.666) ind43(1)
                    else ind43(2)

                }

                ind433

              case _ => 6

            }

            I43

          case 4 =>

            //println(s"goal difference filter - 4")
            //Thread.sleep(10000)

            val gf1 = goalDif(2 * ind4(0))
            val gf2 = goalDif(2 * ind4(1))
            val gf3 = goalDif(2 * ind4(2))
            val gf4 = goalDif(2 * ind4(3))

            //println(s"$gf1,$gf2, $gf3, $gf4")

            val I44: Int = List(gf1, gf2, gf3, gf4).count(_ == math.max(gf1,math.max(gf2, math.max(gf3, gf4)))) match {

              case 1 =>

                math.max(gf1, math.max(gf2, math.max(gf3, gf4))) match {

                  case `gf1` => ind4(0)
                  case `gf2` => ind4(1)
                  case `gf3` => ind4(2)
                  case `gf4` => ind4(3)
                  case _ => 6
                }

              case 2 =>

               // println(s"442 - direct match")

                val ind442: Seq[Int] = List(gf1, gf2, gf3, gf4).zipWithIndex.filter(_._1 == math.max(gf1, math.max(gf2, math.max(gf3, gf4)))).map(_._2)

                //println(s"${check_m(matches, ind442(0), ind442(1))}")

                val index = if(check_m(matches, ind442.head, ind442(1)) == 3) ind442.head
                else if(check_m(matches, ind442.head, ind442(1)) == 0) ind442(1)
                else //random promotion
                  if(r.nextFloat() < 0.5) ind442.head
                  else ind442(1)

                index

              case 3 =>

                //println(s"same points, goal balance and goals scored for three teams; check points in direct matches; reduced table")

                val ind443: Seq[Int] = List(gf1, gf2, gf3, gf4).zipWithIndex.filter(_._1 == math.max(gf1, math.max(gf2, math.max(gf3, gf4)))).map(_._2)

                val ind443_0_points: Int =
                  check_m(matches, ind443.head, ind443(1)) + check_m(matches, ind443.head, ind443(2))
                val ind443_1_points: Int =
                  if(check_m(matches, ind443.head, ind443(1)) == 1) 1 else 3 - check_m(matches, ind443.head, ind443(1)) + check_m(matches, ind443(1), ind443(2))
                val ind443_2_points: Int =
                  (if(check_m(matches, ind443.head, ind443(2)) == 1) 1 else 3 - check_m(matches, ind443.head, ind443(2))) +
                    (if(check_m(matches, ind443(1), ind443(2)) == 1) 1 else 3 - check_m(matches, ind443(1), ind443(2)))

                /*println(s"check, " +
                  s"${check_m(matches, ind443(0), ind443(1)) + check_m(matches, ind443(0), ind443(2))}" +
                  s"${if(check_m(matches, ind443(0), ind443(1)) == 1) 1 else 3 - check_m(matches, ind443(0), ind443(1)) + check_m(matches, ind443(1), ind443(2))}" +
                  s"${(if(check_m(matches, ind443(0), ind443(2)) == 1) 1 else 3 - check_m(matches, ind443(0), ind443(2))) +
                    (if(check_m(matches, ind443(1), ind443(2)) == 1) 1 else 3 - check_m(matches, ind443(1), ind443(2)))}"
                )*/

                val index = List(ind443_0_points, ind443_1_points, ind443_2_points).iterator.count(p => p == math.max(ind443_0_points, math.max(ind443_1_points, ind443_2_points))) match {

                  case 1 =>

                    val ind4431: Int = math.max(ind443_0_points, math.max(ind443_1_points, ind443_2_points)) match {

                      case `ind443_0_points` => ind443.head
                      case `ind443_1_points` => ind443(1)
                      case `ind443_2_points` => ind443(2)
                      case _ => 6
                    }
                    ind4431

                  case 2 =>

                    val ind4432 = List(ind443_0_points, ind443_1_points, ind443_2_points).zipWithIndex.filter(_._1 == math.max(ind443_0_points, math.max(ind443_1_points, ind443_2_points))).map(_._2)

                    val index = if(check_m(matches, ind4432.head, ind4432(1)) == 3) ind4432.head
                    else if(check_m(matches, ind4432.head, ind4432(1)) == 0) ind4432(1)
                    else //random promotion
                      if(r.nextFloat() < 0.5) ind4432.head
                      else ind4432(1)

                    ind443(index)

                  case 3 => //random promotion

                    val rn = r.nextFloat()

                    if(rn < 0.333) ind443.head
                    else if(rn < 0.666) ind443(1)
                    else ind443(2)

                }

                index

              case 4 => //random promotion

                val rn = r.nextFloat()

                if(rn < 0.25) ind4(0)
                else if(rn < 0.5) ind4(1)
                else if(rn < 0.75) ind4(2)
                else ind4(3)

              case _ => 6


            }

            I44

          case _ => 6

        }

        I4
    }

    winner

  }

  //val A = fill(4, 3, 2)
  //val B = fill(3, 3, 3)


  private val n = 1


  /*  for (j <- 1 until 10) {
      val t1 = t(j, j, j)
      for (k <- j until 10) {
        val t2 = t(k,k,k)
        var avg_g = 0.0
        for (i <- 0 until n)
          //avg_g += result(fill(t1, math.max(t1.max, t2.max), math.min(t1.min, t2.min)), fill(t2, math.max(t1.max, t2.max), math.min(t1.min, t2.min)))
          avg_g += result(t1,t2)
        println(s"$j $k ${avg_g / n}")
      }
    }*/


  private val tH1 = t(8, 8, 7)
  private val tH2 = t(7, 8, 9)
  private val tH3 = t(7, 6, 10)
  private val tH4 = t(5, 6, 5)

  private val tA1 = t(8, 9, 7)
  private val tA2 = t(3, 2, 1)
  private val tA3 = t(8, 9, 8)
  private val tA4 = t(5, 4, 7)

  private val tB1 = t(10, 10, 11)
  private val tB2 = t(10, 10, 10)
  private val tB3 = t(4, 7, 5)
  private val tB4 = t(4, 2, 6)

  private val tC1: t = t(10, 10, 12)
  private val tC2 = t(5, 6, 3)
  private val tC3 = t(9, 6, 9)
  private val tC4 = t(3, 4, 4)

  private val tD1 = t(9, 10, 12)
  private val tD2 = t(5, 7, 8)
  private val tD3 = t(7, 10, 10)
  private val tD4 = t(4, 5, 7)

  private val tE1 = t(8, 8, 7)
  private val tE2 = t(6, 10, 5)
  private val tE3 = t(10, 10, 12)
  private val tE4 = t(6, 3, 1)

  private val tF1 = t(10, 10, 10)
  private val tF2 = t(6, 5, 6)
  private val tF3 = t(1, 4, 5)
  private val tF4 = t(5, 6, 9)

  private val tG1 = t(10, 11, 11)
  private val tG2 = t(10, 11, 11)
  private val tG3 = t(1, 1, 1)
  private val tG4 = t(5, 7, 5)


  case class tInfo(n: String, p: t)

  //var nt = List(tInfo("Polska", tH1), tInfo("Senegal", tH2), tInfo("Columbia", tH3), tInfo("Japonia", tH4))
  //var nt = List(tInfo("Rosja", tA1), tInfo("Arabia S.", tA2), tInfo("Urugwaj", tA3), tInfo("Egipt", tA4))
  //var nt = List(tInfo("Portugalia", tB1), tInfo("Hiszpania", tB2), tInfo("Maroko", tB3), tInfo("Iran", tB4))
  //var nt = List(tInfo("Francja", tC1), tInfo("Australia", tC2), tInfo("Dania", tC3), tInfo("Peru", tC4))
  //var nt = List(tInfo("Argentyna", tD1), tInfo("Nigeria", tD2), tInfo("Chorwacja", tD3), tInfo("Islandia", tD4))
  //var nt = List(tInfo("Szwajcaria", tE1), tInfo("Serbia", tE2), tInfo("Brazylia", tE3), tInfo("Kostaryka", tE4))
  //var nt = List(tInfo("Niemcy", tF1), tInfo("Szwecja", tF2), tInfo("Korea", tF3), tInfo("Meksyk", tF4))
  //var nt = List(tInfo("Anglia", tG1), tInfo("Belgia", tG2), tInfo("Panama", tG3), tInfo("Tunezja", tG4))

  private val group_list: Array[List[tInfo]] = Array(
    List(tInfo("Rosja", tA1), tInfo("Arabia S.", tA2), tInfo("Urugwaj", tA3), tInfo("Egipt", tA4)),
    List(tInfo("Portugalia", tB1), tInfo("Hiszpania", tB2), tInfo("Maroko", tB3), tInfo("Iran", tB4)),
    List(tInfo("Francja", tC1), tInfo("Australia", tC2), tInfo("Dania", tC3), tInfo("Peru", tC4)),
    List(tInfo("Argentyna", tD1), tInfo("Nigeria", tD2), tInfo("Chorwacja", tD3), tInfo("Islandia", tD4)),
    List(tInfo("Szwajcaria", tE1), tInfo("Serbia", tE2), tInfo("Brazylia", tE3), tInfo("Kostaryka", tE4)),
    List(tInfo("Niemcy", tF1), tInfo("Szwecja", tF2), tInfo("Korea", tF3), tInfo("Meksyk", tF4)),
    List(tInfo("Anglia", tG1), tInfo("Belgia", tG2), tInfo("Panama", tG3), tInfo("Tunezja", tG4)),
    List(tInfo("Polska", tH1), tInfo("Senegal", tH2), tInfo("Columbia", tH3), tInfo("Japonia", tH4))
  )

  private val teamList: Seq[String] = group_list.flatMap(x => x.map(y => y.n)).toSeq

  //println(teamList)

  private def single_group(group_list: Array[List[tInfo]], group_id: Int): (Int, Int, Int) ={

    val nt: Seq[tInfo] = group_list(group_id)

    val po = scala.collection.mutable.ArrayBuffer.empty[Int]
    val g = scala.collection.mutable.ArrayBuffer.empty[Int]
    val b = scala.collection.mutable.ArrayBuffer.empty[Int]
    val m = scala.collection.mutable.ArrayBuffer.empty[Int]
    val g1 = scala.collection.mutable.ArrayBuffer.empty[Int]
    val g2 = scala.collection.mutable.ArrayBuffer.empty[Int]

    po.addAll(List(0,0,0,0))
    g.addAll(List(0, 0, 0, 0, 0, 0, 0, 0))
    b.addAll(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    m.addAll(List(0, 0, 0, 0, 0, 0))//0 -> 01; 1 -> 02; 2 -> 03; 3 -> 12; 4 -> 13; 5 -> 23
    g1.addAll(List(0, 0, 0, 0, 0, 0))//0 -> 01; 1 -> 02; 2 -> 03; 3 -> 12; 4 -> 13; 5 -> 23
    g2.addAll(List(0, 0, 0, 0, 0, 0))//0 -> 01; 1 -> 02; 2 -> 03; 3 -> 12; 4 -> 13; 5 -> 23



    def add_m(id_1: Int, id_2: Int, p: Int): Unit = {

      if(id_1 == 0 && id_2 == 1) m(0) += p
      else if(id_1 == 0 && id_2 == 2) m(1) += p
      else if(id_1 == 0 && id_2 == 3) m(2) += p
      else if(id_1 == 1 && id_2 == 2) m(3) += p
      else if(id_1 == 1 && id_2 == 3) m(4) += p
      else if(id_1 == 2 && id_2 == 3) m(5) += p

    }

    def add_g1(id_1: Int, id_2: Int, r: (Int, Int)): Unit = {

      if (id_1 == 0 && id_2 == 1) g1(0) += r._1
      else if (id_1 == 0 && id_2 == 2) g1(1) += r._1
      else if (id_1 == 0 && id_2 == 3) g1(2) += r._1
      else if (id_1 == 1 && id_2 == 2) g1(3) += r._1
      else if (id_1 == 1 && id_2 == 3) g1(4) += r._1
      else if (id_1 == 2 && id_2 == 3) g1(5) += r._1

    }


    def reduce_m(m: ArrayBuffer[Int], W: Int) = {

      if(W == 0) m.drop(3)
      else if(W == 1) m.slice(1, 3) ++ m.drop(5)
      else if(W == 2) m.take(1) ++ m.slice(2, 3) ++ m.slice(4, 5)
      else m.take(2) ++ m.slice(3, 4)


    }

    for (_ <- 0 until n) {

      /*add_m(0,1,3)
      add_m(0,2,1)
      add_m(0,3,0)

      add_m(1,2,3)
      add_m(1,3,1)

      add_m(2,3,3)

      g(0) += 4
      g(1) += 7
      g(2) += 4
      g(3) += 3
      g(4) += 4
      g(5) += 3
      g(6) += 4
      g(7) += 3

      po(0) += 4
      po(1) += 4
      po(2) += 4
      po(3) += 4

      b(0) = 1
      b(1) = 1
      b(2) = 1
      b(3) = 1
      b(4) = 1
      b(5) = 1
      b(6) = 1
      b(7) = 1
      b(8) = 1
      b(9) = 1
      b(10) = 1
      b(11) = 1
*/


      for (i <- 0 until 4)
        for (j <- i + 1 until 4) {
          //println(s"${nt(i).n} - ${nt(j).n}")
          //val r: (Int, Int) = result(nt(i).p, nt(j).p)
          val r: (Int, Int) = regular_time(nt(i).p, nt(j).p)

          val p = {
            if (r._1 > r._2) (3, 0)
            else if (r._1 == r._2) (1, 1)
            else (0, 3)
          }
          add_m(i,j,p._1)
          val w = {
            if (r._1 > r._2) (1, 0, 0)
            else if (r._1 == r._2) (0, 1, 0)
            else (0, 0, 1)
          }
          g(2 * i) += r._1
          g(2 * i + 1) += r._2
          g(2 * j) += r._2
          g(2 * j + 1) += r._1

          po(i) += p._1
          po(j) += p._2

          b(3 * i) += w._1
          b(3 * i + 1) += w._2
          b(3 * i + 2) += w._3
          b(3 * j) += w._3
          b(3 * j + 1) += w._2
          b(3 * j + 2) += w._1
        }


    }

    /*println(g)
    println(po)
    println(s"goal balance: $b")
    println(s"matches_full: $m")*/

    /*po.zipWithIndex.foreach(i => {

      val eqi = po.zipWithIndex.filter(p => p._1 == po(i._2)).map(_._2)
      println(s" $i, $eqi,${if(eqi.length>1) eqi.filter(p => p != i._2).foreach(j => check_m(m,math.min(i._2,j),math.max(i._2,j))) else -1}")
    })*/

    //g = ArrayBuffer(2, 3, 4, 6, 8, 1, 2, 6)
    //
    // po = ArrayBuffer(3, 3, 9, 3)


    val W: Int = pickWinner(po, g, m)

    //println(s"${po.take(W) ++ po.drop(W + 1)}, ${g.take(2 * W) ++ g.drop(2 * W + 2)}")

    val m_reduced = reduce_m(m, W)

    //println(s"m_reduced: $m_reduced, ${m_reduced.length}")

    val RU = pickWinner(po.take(W) ++ po.drop(W + 1), g.take(2 * W) ++ g.drop(2 * W + 2), m_reduced)

    //println(s"$W, $RU")

    val RUC = if (RU < W) RU
    else RU + 1

    //println(s"$W, $RUC")
    //println(s"Qualified: ${nt(W).n}, ${nt(RUC).n}")

    (group_id, W, RUC)
  }

  private def SG(group_list: Array[List[tInfo]], group_id: Int, qualified: Seq[(Int, Int, Int)]): Seq[(Int, Int, Int)] = {

    if(group_id == 8) qualified
    else Seq(single_group(group_list, group_id)) ++ SG(group_list, group_id + 1, qualified)

  }

  for(_ <- 0 until 1000){

    val group_stage_results: Seq[(Int, Int, Int)] = SG(group_list, 0, Seq.empty)
    //group_stage_results -> group number, id_winner, id_runner_up

    def group_qualifiers(
                          group_id: Int, group_list: Array[List[tInfo]],
                          test: Seq[(Int, Int, Int)]): ((String, t), (String, t)
      ) = {
      val winner    = (group_list(test(group_id)._1)(test(group_id)._2).n,group_list(test(group_id)._1)(test(group_id)._2).p)
      val runner_up = (group_list(test(group_id)._1)(test(group_id)._3).n,group_list(test(group_id)._1)(test(group_id)._3).p)

      (winner, runner_up)
    }

    val A1: (String, t) = group_qualifiers(0, group_list, group_stage_results)._1
    val A2: (String, t) = group_qualifiers(0, group_list, group_stage_results)._2

    val B1: (String, t) = group_qualifiers(1, group_list, group_stage_results)._1
    val B2: (String, t) = group_qualifiers(1, group_list, group_stage_results)._2

    val C1: (String, t) = group_qualifiers(2, group_list, group_stage_results)._1
    val C2: (String, t) = group_qualifiers(2, group_list, group_stage_results)._2

    val D1: (String, t) = group_qualifiers(3, group_list, group_stage_results)._1
    val D2: (String, t) = group_qualifiers(3, group_list, group_stage_results)._2

    val E1: (String, t) = group_qualifiers(4, group_list, group_stage_results)._1
    val E2: (String, t) = group_qualifiers(4, group_list, group_stage_results)._2

    val F1: (String, t) = group_qualifiers(5, group_list, group_stage_results)._1
    val F2: (String, t) = group_qualifiers(5, group_list, group_stage_results)._2

    val G1: (String, t) = group_qualifiers(6, group_list, group_stage_results)._1
    val G2: (String, t) = group_qualifiers(6, group_list, group_stage_results)._2

    val H1: (String, t) = group_qualifiers(7, group_list, group_stage_results)._1
    val H2: (String, t) = group_qualifiers(7, group_list, group_stage_results)._2

    //val B1: (String, t) = (group_list(test(1)._1)(test(1)._2).n,group_list(test(1)._1)(test(1)._2).p)
    //val B2: (String, t) = (group_list(test(1)._1)(test(1)._3).n,group_list(test(1)._1)(test(1)._3).p)

    r1_16_tmp ++= Seq(A1._1,A2._1)
    r1_16_tmp ++= Seq(B1._1,B2._1)
    r1_16_tmp ++= Seq(C1._1,C2._1)
    r1_16_tmp ++= Seq(D1._1,D2._1)
    r1_16_tmp ++= Seq(E1._1,E2._1)
    r1_16_tmp ++= Seq(F1._1,F2._1)
    r1_16_tmp ++= Seq(G1._1,G2._1)
    r1_16_tmp ++= Seq(H1._1,H2._1)

    val A1B2 = play_off(A1, B2)

    println(A1B2)

    //test.foreach(x => println(s"${group_list(x._1)(x._2).n}, ${group_list(x._1)(x._3).n}"))
    //println(A1,A2)

  }

  //println(r1_16_tmp)

  private val cc = r1_16_tmp.result()

  private val teams_n_1_16: Seq[(String, Int)] = teamList.map(teamName => (teamName, cc.count(p => p == teamName)))

  println(teams_n_1_16)

  //println(s"${teams_n_1_16(0)._1} -> ${teams_n_1_16(0)._2/1000.0}")

  //result(tG1, tG2, 10000)

}
