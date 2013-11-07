reqT-gui
========

A prototype GUI frontend to reqT

Compilation
===========


Prerequisites
-------------

You need to have Java and Scala installed.

Create a bin/ and lib/ folder at the top level.

Add JaCoP-3.2.jar and reqT.jar to the lib folder.

Add reqT.jar to the bin folder.

Create jar
----------

Find your [YOUR_jar.exe_PATH]. In Windows it may be: C:/Program Files/Java/jdk1.7.0_40/bin/jar.exe

Execute the following commands.
	* cd to top folder.
	* scalac -cp "./lib/JaCoP-3.2.jar;./lib/reqT.jar" ./src/main/scala/gui/* -d bin
	* "[YOUR_jar.exe_PATH]" -cfm ReqTGUI.jar MANIFEST.MF -C bin .

Runnable jar in Eclipse
-----------------------

	* Install Scala plugin in Eclipse.
	* Create a Scala project. It will be the Scala wrapper of ReqTGUI.
	* Add a object that have a main which do what is wished to do.
	* Add external jars scala-compiler.jar, scala-reflect.jar, reqT.jar, ReqTGUI.jar and scala-library.jar
	* Create a Java project. It will be the Java wrapper.
	* Add jar scala-library.jar
	* Add the Scala wrapper as a project to the Java project.
	* Create a Java main class that initiates the Scala wrapper object.
	* Run the Java application.
	* Export Java wrapper as Runnable jar that includes all jars.
	
Example of a object that starts the laboratory main object:

<code>
package main
import gui.ReqTGUI

object MyMain {
  def main(args: Array[String]) {
	  gui.laboratoryMain.start()
  }
}
</code>

Example of a Java wrapper:

<code>
package javamain;

import main.MyMain;

public class Wrapping {
	
	public static void main(String[] e){
		MyMain.main(e);
	}
	
}
</code>