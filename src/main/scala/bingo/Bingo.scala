package bingo

import bingo.Bingo.arvoNumero
import scala.collection.mutable.ListBuffer

/*
BINGO-PELI
Pelissä on kaksi pelaajaa: ihmispelaaja ja tietokonepelaaja
Molemmille pelaajille arvotaan alussa bingolappu (5x5 ruudukko), joka sisältää numeroita väliltä 1 ja 75. Lapun keskimmäinen ruutu on valmiiksi 0.
Joka kierros arvotaan yksi numero väliltä 1-75.

TOTEUTUS NRO1: Pelaajan tarvitsee vain painaa enter, jolloin peli etenee. Nollat merkitään ja bingolappu tarkistetaan pelaajan puolesta.
TOTEUTUS NRO2 (kommentoituna, huomattavasti hitaampi ja työläämpi pelata): Pelaaja tarkistaa itse löytyykö bingolapusta arvottu numero,
ilmoittaa sen sijainnin, ja ilmoittaa myös jos lapusta löytyy bingo.

Pelin voittaa se, kumpi saa ensimmäisenä viiden suoran 0-numeroita
 */

object Bingo {
  def main(args: Array[String]): Unit ={
    println("BINGO-PELI" +
      "\nPelin tarkoituksena on saada viiden suora nollia pelilapulla (vaaka- tai pystytasossa, tai vinottain)" +
      "\nVastustajanasi toimii tietokonepelaaja\n")
    // Pelin alustus, luodaan bingolaput
    val lappu1 = new BingoLappu()
    val lappu2 = new BingoLappu()
    lappu1.luoBingoLappu()
    lappu2.luoBingoLappu()
    println("Bingolappusi:")
    lappu1.tulostaLappu()
    var jatkuu = true
    var kaytetytNumerot = ListBuffer(0) // lista jo arvotuista numeroista
    var vast = ""

    // Peli alkaa
    while (jatkuu) {

      // Arvotaan numero, ja lisätään se käytetyiden numeroiden listaan
      var numero = arvoNumero(kaytetytNumerot)
      kaytetytNumerot += numero
      println("\nArvottu numero: " + numero)

      // TOTEUTUS NRO1 (nopeampi pelata)
      vast = scala.io.StdIn.readLine("Paina enter tarkistaaksesi löytyykö lapultasi numeroa " + numero + ", valitse 0 lopettaaksesi pelin")

      vast match {
        case "0" => jatkuu = false
        case _ =>
          // Tarkistetaan löytyykö lapulta arvottua numeroa, ja merkitään se nollaksi, jos löytyy
          for (i <- 0 until 5) yield {
            for (j <- 0 until 5) yield {
              if (lappu1.annaLapunNumero(i, j) == numero) {
                lappu1.merkitseRuutu(i + 1, j + 1)
                println("Lapustasi löytyi numero " + numero)
              }
            }
          }

          // Tarkistetaan onko lapulla voittoa, eli viiden suoraa
          if (lappu1.onkoVoittoa()) {
            jatkuu = false
            println("\nVOITIT PELIN!")
          }
      }

       //TOTEUTUS NRO2 (hitaampi pelata):
       //pelaajan tulee itse tarkistaa löytyykö arvottu numero lapulta, merkitä se, ja tarkistaa onko hänellä bingoa
//      val vastaus = scala.io.StdIn.readLine("Valitse:\n" +
//          "0 BINGO! Jos lapussasi on viiden suora nollia.\n" +
//          "1 Jos bingolapustasi löytyy arvottu numero.\n" +
//          "Muutoin paina enter.")
//          vastaus match {
//            case "0" => if (lappu1.onkoVoittoa()) {jatkuu = false
//              println("VOITIT PELIN!")} // BINGO, tarkista onko viiden suora
//            else println("Ei voittoa, peli jatkuu")
//            case "1" => // Käyttäjä haluaa merkitä numeron
//              val rivi = scala.io.StdIn.readLine("Anna rivi (1-5).")
//              rivi match {
//                // Tarkistetaan onko annettu riviksi numero 1-5.
//                case "1" | "2" | "3" | "4" | "5" => val sarake = scala.io.StdIn.readLine("Anna sarake (1-5).")
//                  sarake match {
//                    // Tarkistetaan onko annettu sarakkeeksi numero 1-5.
//                    case "1" | "2" | "3" | "4" | "5" =>
//                      // Tarkistetaan onko käyttäjän antamassa ruudussa arvottu numero. Jos on, ruutu merkitään nollaksi.
//                      if (lappu1.annaLapunNumero(rivi.toInt - 1, sarake.toInt - 1) == numero) lappu1.merkitseRuutu(rivi.toInt, sarake.toInt)
//                      else println("Riviltä " + rivi + " ja sarakkeesta " + sarake + " ei löytynyt numeroa " + numero + ".")
//                    case _ => println("Virheellinen valinta.")
//                  }
//                case _ => println("Virheellinen valinta.")
//              }
//            case _ => println("Arvotaan seuraava numero...")
//      }

      // Tietokonepelaajan vuoro
      if (jatkuu) jatkuu = lappu2.tietokonepelaajaPelaa(numero)

      // Lappujen tulostus vuorojen jälkeen
      println("Lappusi:")
      lappu1.tulostaLappu()
      println("Tietokonepelaajan lappu:")
      lappu2.tulostaLappu()
    }
  }

  // Arpoo random-numeron 1-75
  // kaytetyt-lista kuvaa listaa jo arvotuista numeroista, jotta ei arvota yhtä numeroa useampaan kertaan
  def arvoNumero(kaytetyt: ListBuffer[Int]): Int ={
    val random = new scala.util.Random
    var numero = random.nextInt(74) + 1
    // arpoo uuden numeron, jos random-numero on jo arvottu aiemmin
    kaytetyt.foreach(x => if (x == numero) numero = arvoNumero(kaytetyt))
    numero
  }
}

class BingoLappu {

  // luo 5x5 ruudukon
  var lappu = Array.ofDim[Int](5, 5)

  def luoBingoLappu(): Array[Array[Int]] = {
    // luo listan, johon tallennetaan jo käytetyt numerot
    var kaytetytNumerot = ListBuffer(0)
    // jokaiseen taulukon 5x5 osioon arvotaan eri numero
    for (i <- 0 until 5) yield {
      for (j <- 0 until 5) yield {
        if (i == 2 && j == 2) lappu(i)(j) = 0 else lappu(i)(j) = arvoNumero(kaytetytNumerot)
        kaytetytNumerot += lappu(i)(j)
      }
    }
    lappu
  }

  // Antaa numeron, joka löytyy pyydetyltä riviltä ja sarakkeelta
  def annaLapunNumero(rivi: Int, sarake: Int): Int = lappu(rivi)(sarake)

  // Tulostaa bingolapun
  def tulostaLappu(): Unit = lappu.foreach(x => println(x.mkString(" | ")))

  // Merkitsee halutun ruudun nollaksi
  def merkitseRuutu(rivi: Int, sarake: Int): Unit = lappu(rivi - 1)(sarake - 1) = 0

  // Tietokonepelaajan toiminta vuoron aikana
  def tietokonepelaajaPelaa(arvottuNumero: Int): Boolean = {
    // Tarkistetaan löytyykö arvottua numeroa pelaajan lapulta, ja merkitään se nollaksi,  jos löytyy
    for (i <- 0 until 5) yield {
      for (j <- 0 until 5) yield {
        if (lappu(i)(j) == arvottuNumero) lappu(i)(j) = 0
      }
    }
    // Tarkistetaan onko lapulla voittoa, jos on, tietokonepelaaja voittaa ja peli päättyy
    if (onkoVoittoa()) {
      println("\nTIETOKONEPELAAJA VOITTI PELIN!")
      false
    }
    else true
  }

  // Tarkistetaan löytyykö viiden suoraa vaaka- tai pystytasossa tai vinottain
  def onkoVoittoa(): Boolean = {
    if (voittoJollainRivilla(lappu) || voittoJollainSarakkeella(lappu) || voittoVinottain(lappu)) true
    else false
  }

  def voittoJollainRivilla(lappu: Array[Array[Int]]): Boolean = lappu.exists(rivi => voittoRivilla(rivi))

  def voittoRivilla(rivi: Array[Int]): Boolean = rivi.forall(x => x == 0)

  def voittoJollainSarakkeella(lappu: Array[Array[Int]]) : Boolean = voittoJollainRivilla(lappu.transpose)

  def voittoVinottain(lappu: Array[Array[Int]]): Boolean = {
    if (lappu(0)(0) == 0 && lappu(1)(1) == 0 && lappu(2)(2) == 0 && lappu(3)(3) == 0 && lappu(4)(4) == 0) true
    if (lappu(4)(0) == 0 && lappu(3)(1) == 0 && lappu(2)(2) == 0 && lappu(1)(3) == 0 && lappu(0)(4) == 0) true
    else false
  }
}