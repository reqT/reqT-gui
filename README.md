reqT-prototype-gui-constr
========

This repo is a requirements engineering tool prototype developed during the master
thesis by Oskar Präntare and Joel Johansson in the spring of 2013. It is an extension built
on the free requirements modelling tool reqT ( www.reqt.org ) maintained by Björn Regnell at 
Lund University, LTH, Sweden.

Compilation
===========


Prerequisites
-------------

You need to have Java 1.7 and Scala 2.10 installed.

Create a bin/ folder at the top level.

The lib/ folder should contain JaCoP-3.2.jar and reqT.jar to the lib folder.

Copy the reqT.jar to the bin folder.

(The reqT.jar is compiled based on the reqT v2.3.x branch)

Create jar
----------

Find your [YOUR_jar.exe_PATH]. In Windows it may be: C:/Program Files/Java/jdk1.7.0_40/bin/jar.exe

Execute the following commands.
	* cd to top folder.
	* `scalac -cp "./lib/JaCoP-3.2.jar;./lib/reqT.jar" ./src/main/scala/gui/* -d bin`
	* `cp locktrans15.png ./bin/gui/locktrans15.png`
	* `cp locktrans25.png ./bin/gui/locktrans25.png`
	* "[YOUR_jar.exe_PATH]" -cfm ReqTGUI.jar MANIFEST.MF -C bin .

Execute
----------
  
To execute the ReqTGUI from command line inside the top folder as above:
  * `scala -toolcp bin/reqT.jar -cp ReqTGUI.jar`
  * Or if you don't want to use the jar, but the class files in bin:
  * Windows: `scala -toolcp "lib/reqT.jar;bin" -cp "lib/reqT.jar;bin"`
  * Unix: `scala -toolcp lib/reqT.jar:bin -cp lib/reqT.jar:bin`
  * Then after scala has started, type at the `scala>` prompt:
  
    * `reqt.init($intp)`
    * `gui.ReqTGUI()`  and don't forget the () 
    * use `exit`to exit and not `:q` as the latter may cause hanging thread

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

```
package main
import gui.ReqTGUI

object MyMain {
  def main(args: Array[String]) {
	  gui.laboratoryMain.start()
  }
}
```

Example of a Java wrapper:

```
package javamain;

import main.MyMain;

public class Wrapping {
	
	public static void main(String[] e){
		MyMain.main(e);
	}
	
}
```
