package cse250.pa0
/**
 * cse250.pa0.AssessmentDataProcessor
 *
 * Copyright 2021 Oliver Kennedy (okennedy@buffalo.edu)
 *           2021 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: jamesaqu
 * Person#: 50130376
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
/**
 * Sources used:
 * https://www.scala-lang.org/api/current/index.html
 * https://docs.scala-lang.org/
 * https://docs.scala-lang.org/overviews/scala-book/arraybuffer-examples.html
 * https://docs.scala-lang.org/overviews/scala-book/try-catch-finally.html#inner-main
 */

import cse250.objects.SolarInstallation

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object DataProcessor {

  def splitArrayToRowArray(splitHeaderRow: Array[String]): Array[String] = {
  //  var TempArray: List[String] = List()
    var ArrayBuff: ArrayBuffer[String] = ArrayBuffer[String]()
//    ArrayBuff += "Hello"
//    ArrayBuff += "There"
//    for (elem <- ArrayBuff){
//      count += 1
//      println(elem + " " + count.toString)
//    }
 //   println("This is arraybuff at index 0: " + ArrayBuff(0))
 //   println("This is arraybuff at index 0: " + ArrayBuff(1))
    // to fix this I am going to need to use a new data structure instead of a list
    // use ArrayBuffer
    // or a pre-made Array like Array[String] = Array[](31)

    // println(splitHeaderRow(1))
    var quotedString = ""
    //  print(s"quotedString: " + quotedString + '\n')
    var lists: List[Int]= List()
    var newList = lists :+ 0
    for (row <- splitHeaderRow) {

      /*
      What if the row contains """Business, Work, Business""" ????
      */

      if (row.startsWith("\"") && quotedString == "" ) { // if row has a quote
        // println("This row contains quotes " + row)
        quotedString += row + ","// change the qS variable to hold that element
        //      println(quotedString)
      }
      else if (row.contains("\"") ) {
        quotedString += row
        var q = quotedString.replace("\"", "")
        ArrayBuff += q
        q = ""
        quotedString = ""
      }
      else if (quotedString.contains("\"\"\"")) {
        quotedString += row
      }

      else if (!row.contains("\"") && quotedString != ""){ // meaning we only enter this elif when we have no quotes

        quotedString += row + ","
      }
      else {
       // TempArray :+= row
        ArrayBuff += row
      }



    }
    //  println("The length of the list is :" + TempArray.length)
    // println(TempArray.isEmpty)
   // TempArray.toArray
    ArrayBuff.toArray
  }

  def rowArrayToSolarInstallation(rowData: Array[String]): SolarInstallation = {
    /*
    A Solar Installation represents a single data record
    The keys of the Solar Installation are the headers of the columns
    The values are based on the correlating column.

    Maybe I can create a dictionary with the required headers from a Solar Installation object since that data
    is already stored in the object...yes
    Because for each line that we loop through, the rowData is going to be different, so to constantly use the
    same object that contains all the headers, we can assign the value of each column to the matching header.

    In order for this to really work we will need to determine the array index of the matching column. Then
    call rowData(n) on the corresponding column number to correctly create a dictionary with the data

     */
    val SI = new SolarInstallation
    // print(SI)
    val SI_Object = SolarInstallation
    //  print(rowData(0) + '\n')
    //  println(SI_Object.REQUIRED_HEADERS) // ok so each header in the set == the column number
    //   println(SI_Object.HEADERS) // we need to compare the headers to the required headers to make sure the column
    // number is equivalent.
    //   println(SI_Object.HEADERS.indices)
    for (i <- SI_Object.REQUIRED_HEADERS.indices){
      for (j <- SI_Object.HEADERS.indices) {
        if ( SI_Object.REQUIRED_HEADERS(i) == SI_Object.HEADERS(j)) {
          SI.fields += (SI_Object.REQUIRED_HEADERS(i) -> rowData(j))
        }
      }
      //   println(SI_Object.HEADERS(i))
    }

    // I need to compare the two list so I know what the correct column number is on each iteration.

    // for example:
    /*
    column(2) in Req Headers == city
    column(3) in headers     == city
    headers is what the rowData CSV array will be.
    Meaning I can't call column(2) on the rowData because I won't be given the right data,

    If headers(2) != req_headers(2)
      don't put it in the dictionary
     else if headers() == req_headers( same column)
      then yes insert that data into the dictionary

      for (i <- req_headers)
        for (j <- headers)
          if i == j
            dictionary = { i -> req_headers(j) }
     */
    //  //  print(rowData(0).isInstanceOf[String])
    //    for (row <- rowData) {
    //    //    print(rowData(i).isInstanceOf[String])
    //        for (req_header <- SI_Object.REQUIRED_HEADERS) {
    //          // how to remove the underscore...
    //         //  val head = (header.toLowerCase.replace('_', ' ') )
    //      //     val row = (rowData(0).toLowerCase )
    //      //     val bool = header.toLowerCase.replace('_', ' ') == rowData(i).toLowerCase
    //           if (req_header.toLowerCase.replace('_', ' ') == row.toLowerCase){
    //             SI.fields += (req_header -> row) // this is wrong
    //           }
    //        }
    //    }
    //    println(SI.fields.keys)
    //    println("these are the values : " + SI.fields.values)
    //    var length = SI.fields.keys
    //    println("number of keys : " + length.size)
    //println(SI.toString)
    SI
  }
  // prepend = +:=
  // append = :+=
  def computeUniqueInverterManufacturers(dataset: Array[SolarInstallation]): Int = {
    val SI = SolarInstallation
    var arrayIndex = 0
    for (head <- SI.REQUIRED_HEADERS.indices) {
      if ( SI.REQUIRED_HEADERS(head) == "Primary Inverter Manufacturer" ) {
        arrayIndex = head
      }
    }
    var PrimaryList: ListBuffer[String] = ListBuffer()
    for (d <- dataset) {
      PrimaryList :+= d.fields.getOrElse("PRIMARY_INVERTER_MANUFACTURER", SI.REQUIRED_HEADERS(arrayIndex))
  //    println(d.fields.get("PRIMARY_INVERTER_MANUFACTURER"))
    }
    PrimaryList -= "Primary Inverter Manufacturer"
 //   println(PrimaryList)
//    val bool = PrimaryList.contains("")
  //  println(bool)
    var new_PrimaryList = PrimaryList.distinct
   // println("This is the new Primary List : " + new_PrimaryList)
    val comparator: (String, String) => Boolean = (a:String, b: String) => a < b
    val sortedList = new_PrimaryList.sortWith(comparator)
  //  println(sortedList)
    val newSort = sortedList.drop(1)
 //   println(newSort)
    val returnSet = newSort.toSet
   //   println(returnSet)
      //println(returnSet.size)
    returnSet.size
  }

  def computeTotalExpectedKWHAnnualProduction(dataset: Array[SolarInstallation]): Float = {
    var sum = 0.0f
  //  var SumList: ListBuffer[Int] = ListBuffer()
    for (d <- dataset){
      val Kw: String = d.fields.getOrElse("EXPECTED_KWH_ANNUAL_PRODUCTION", "0")
     // println(Kw)
    try {
      val k = Kw.toFloat
      sum += k
    } catch {
      case e: Exception => 0
    }
    }
    sum
  }
}
