package gui

import java.awt.BorderLayout
import java.awt.Component
import java.awt.Dimension
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.io.File
import scala.swing.Dialog
import javax.swing.JFileChooser
import javax.swing.JFrame
import javax.swing.JMenu
import javax.swing.JMenuBar
import javax.swing.JMenuItem
import javax.swing.JOptionPane
import javax.swing.JPanel
import javax.swing.JTabbedPane
import javax.swing.WindowConstants
import reqt._
import reqt.Entity
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.util.HashSet
import javax.swing.UIManager
import javax.swing.SwingUtilities
import java.util.Observer
import java.util.Observable
import java.awt.image.BufferedImage
import javax.swing.ImageIcon
import javax.swing.plaf.basic.BasicTabbedPaneUI
import java.awt.event.KeyAdapter
import java.awt.event.KeyEvent
import java.awt.event.InputEvent
import java.awt.event.MouseMotionAdapter
import java.awt.Color
import java.awt.Cursor
import java.awt.Insets
import javax.swing.JProgressBar
import javax.swing.JComponent
import javax.swing.AbstractAction
import javax.swing.KeyStroke
import javax.swing.plaf.ColorUIResource
import javax.swing.LookAndFeel
import javax.swing.plaf.metal.MetalTabbedPaneUI
import java.awt.event.WindowListener
import java.awt.event.WindowAdapter
import java.io.PrintWriter
import java.io.Writer
import tools.nsc._
import interpreter._
import tools.nsc.settings._
import tools.nsc.interpreter.ILoop._
import scala.tools.nsc.Settings
import java.io.File
import java.io.OutputStreamWriter

/** laboratoryMain is used as a main class for the lab assignments. **/
object laboratoryMain{
    def start() {
		val settings = new Settings()
		settings.classpath.value = System.getProperty("java.class.path")
		val writer = new PrintWriter((new OutputStreamWriter(Console.out)))
		var intp = new IMain(settings, writer)
		//var m = Some(intp)
		//Model.interpreter = scala.Option[scala.tools.nsc.interpreter.IMain](intp)
		reqt.init(intp)
		println("** Starting Swing GUI ... ")
		Config.EXIT_ON_CLOSE = true
		ReqTGUI()
    }
}

/**
 * A simple GUI which is the base of all GUI components.
 * @param modelName is a String which is the given name of the model.
 * @param view is a View which should be the default View in the GUI.
 * @param container is a ModelContainer which should be used.
 */
class GUI(modelName: String, view: View, container: ModelContainer) extends JFrame {

	/**
	 * Constructor used by GUILauncher and should be default command prompt use of GUI.
	 * Gives the model the default name Unnamed.
	 * @param view is a View which should be the default View in the GUI.
	 */
	def this(view: View) = this(Config.getSettingAfterReload(Config.DEFAULTMODELKEY), view, null)

	/**
	 * Constructor used by GUILauncher and should be default command prompt use of GUI.
	 * Gives the model the default name Unnamed.
	 * @param view is a View which should be the default View in the GUI.
	 */
	def this(name: String, view: View) = this(name, view, null)
	
	val gui = this
	reqt.jacop.Settings.verbose = false
	
	var modelContainers = Map[String, ModelContainer]()
	var names = new HashSet[String]()
	var glassPane : GlassPane = null;
	try{
		glassPane = new GlassPane()
	}catch{
		case e : Exception => glassPane = new GlassPane()
	}

	var mainView = view
	view.setBackground(Color.WHITE)
	val SIZE = new Dimension(1024, 768) // Preset size of the GUI.
	if (view != null) {
		setTitle("ReqT GUI: " + view.toString())
	} else {
		setTitle("ReqT GUI: Test mode")
	}
	setSize(SIZE)
	setPreferredSize(SIZE)
	this.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
	this.addWindowListener(new WindowAdapter() {
		override def windowClosing(e: java.awt.event.WindowEvent) {
			var unsaved = false
			for(i <- 0 until modelTab.getTabCount()) {
				if (modelTab.getIconAt(i) != null) {
					unsaved = true
				} 
			}		
			if (unsaved) {
				if (JOptionPane.showConfirmDialog(gui, "All unsaved changes will be lost. Are you sure you want to exit?", "Unsaved models, exit?", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION) {
					unsaved = false
				}
			}
			if (!unsaved) {
				if(Config.EXIT_ON_CLOSE){
					System.exit(1)
				}else{
					dispose()
				}
			}
		}
	})

	var main = new JPanel(new BorderLayout()) with WITHGREENCOLOR
	var east = new JPanel(new BorderLayout()) with WITHGREENCOLOR
	var modelTab = new ReqTabbedPane() with WITHGRAYCOLOR
	modelTab.setOpaque(true)
	modelTab.setForeground(MENUCOLOR)
	modelTab.setFont(MENUFONT)
	modelTab.setUI(new BasicTabbedPaneUI())
	//modelTab.setUI(new MetalTabbedPaneUI())
	modelTab.addMouseListener(new MouseAdapter() {
		override def mousePressed(e: MouseEvent) {
			val index = modelTab.indexAtLocation(e.getX, e.getY)
			if (index >= 0) {
				listView.updateView(modelTab.getComponentAt(index).asInstanceOf[View].container)
			}
		}
	})
	var menu: JMenu = new JMenu("File") with Tooltip { override def getMessage(): String = { return "GUI interaction." } }
	

	/** Undo & redo action by use of ListView's undoButton to ensure that only the currently visible model is affected **/
	var editMenu = new JMenu("Edit") with Tooltip { override def getMessage(): String = { return "Edit tools." } }
	var undo = new JMenuItem("Undo") with Tooltip {
		override def getMessage(): String = {
			if (isEnabled()) {
				return listView.undoButton.getText()
			} else {
				return ""
			}
		}
		setEnabled(false)
		addActionListener(new ActionListener() {
			def actionPerformed(e: ActionEvent) {
				listView.undoButton.doClick()
			}
		})
	}
	var redo = new JMenuItem("Redo") with Tooltip {
		override def getMessage(): String = {
			if (isEnabled()) {
				return listView.undoButton.getText()
			} else {
				return ""
			}
		}
		setEnabled(false)
		addActionListener(new ActionListener() {
			def actionPerformed(e: ActionEvent) {
				listView.undoButton.doClick()
			}
		})
	}
	var copy = new JMenuItem("Copy") with Tooltip {
		override def getMessage(): String = { return "Click to copy the last selected text." }
		addActionListener(new ActionListener() {
			def actionPerformed(e: ActionEvent) {
				SelectedField.copy()
			}
		})
	}
	var paste = new JMenuItem("Paste") with Tooltip {
		override def getMessage(): String = { return "Click to paste." }
		addActionListener(new ActionListener() {
			def actionPerformed(e: ActionEvent) {
				SelectedField.paste()
			}
		})
	}
	var cut = new JMenuItem("Cut") with Tooltip {
		override def getMessage(): String = { return "Click to cut the last selected text." }
		addActionListener(new ActionListener() {
			def actionPerformed(e: ActionEvent) {
				SelectedField.cut()
			}
		})
	}
	editMenu.add(undo)
	editMenu.add(redo)
	editMenu.addSeparator()
	editMenu.add(copy)
	editMenu.add(paste)
	editMenu.add(cut)
	/** End of Undo*/
	var newModel: JMenuItem = new JMenuItem("New model")
	var close: JMenuItem = new JMenuItem("Close model")
	var open: JMenuItem = new JMenuItem("Open ...")
	var save: JMenuItem = new JMenuItem("Save as ...")
	var quicksave: JMenuItem = new JMenuItem("Save")
	var exit: JMenuItem = new JMenuItem("Exit")
	newModel.addActionListener(new ActionListener() {
		def actionPerformed(e: ActionEvent) {
//			if (JOptionPane.showConfirmDialog(null, "Are you sure you want to create a new model?", "New model?", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
				openModel(Config.getSettingAfterReload(Config.DEFAULTMODELKEY), Model()).unsaved()
//			}
		}
	})
	close.addActionListener(new ActionListener() {
		def actionPerformed(e: ActionEvent) {
			var index = modelTab.getSelectedIndex();
			if (modelTab.getIconAt(index) != null) {
				val choice = JOptionPane.showConfirmDialog(gui, "The model that you want to close is unsaved. Do you want to save the model before closing it?", "Close tab unsaved. Save?", JOptionPane.YES_NO_CANCEL_OPTION)
				if ((choice == JOptionPane.YES_OPTION && saveModel())||choice == JOptionPane.NO_OPTION) {
					closeTab(view)
				}
			} else if (JOptionPane.showConfirmDialog(gui, "Are you sure you want to close the model?", "Close model?", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
				closeTab(view)
			}
		}
	})
	open.addActionListener(new ActionListener() {
		def actionPerformed(e: ActionEvent) {
			loadModel()
		}
	})
	save.addActionListener(new ActionListener() {
		def actionPerformed(e: ActionEvent) {
			saveModel()
		}
	})
	quicksave.addActionListener(new ActionListener() {
		def actionPerformed(e: ActionEvent) {
			quickSave()
		}
	})
	exit.addActionListener(new ActionListener() {
		def actionPerformed(e: ActionEvent) {
			if (JOptionPane.showConfirmDialog(gui, "Do you want to exit?", "Exit?", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
				dispose()
			}
		}
	})
	menu.add(newModel)
	menu.addSeparator()
	menu.add(open)
	menu.add(save)
	menu.add(quicksave)
	menu.addSeparator()
	menu.add(close)
	menu.addSeparator()
	menu.add(exit)

	var about = new JMenu("About") with Tooltip { override def getMessage(): String = { return "GUI information." } }
	var help: JMenuItem = new JMenuItem("Developers")
	about.add(help)
	help.addActionListener(new ActionListener() {
		def actionPerformed(e: ActionEvent) {
			JOptionPane.showMessageDialog(gui, "The reqTGUI was created by Joel Johansson and Oskar Präntare during their master thesis at LTH in 2013.", "About", JOptionPane.INFORMATION_MESSAGE)
		}
	})

	val menuContainer = new JMenuBar()
	var border = new JMenuBar()
	val backButton = new ActionButton("<", new ActionListener() { override def actionPerformed(ev: ActionEvent) { modelContainers.get(modelTab.getTitleAt(modelTab.getSelectedIndex())).head.goBack() } })
	val forwardButton = new ActionButton(">", new ActionListener() { override def actionPerformed(ev: ActionEvent) { modelContainers.get(modelTab.getTitleAt(modelTab.getSelectedIndex())).head.goForward() } })
	val buttonInsets = new java.awt.Insets(0, 0, 0, 0)
	val buttonSize = new Dimension(35, 20)
	backButton.setMargin(buttonInsets);
	forwardButton.setMargin(buttonInsets);
	backButton.setPreferredSize(buttonSize)
	forwardButton.setPreferredSize(buttonSize)
	val buttons = new JPanel(new java.awt.GridLayout(1, 2))
	buttons.add(backButton)
	buttons.add(forwardButton)
	val menuPanelContainer = new JPanel(new BorderLayout())
	val menuPanel = new JPanel(new BorderLayout())
	menuPanel.add(border, BorderLayout.WEST)
	menuPanel.add(buttons, BorderLayout.EAST)
	menuPanelContainer.add(menuPanel, BorderLayout.WEST)
	menuPanelContainer.add(new JMenuBar, BorderLayout.CENTER)
	menuContainer.add(menuPanelContainer)

	border.add(menu)
	border.add(editMenu)

	var exportMenu = new JMenu("Export") with Tooltip { override def getMessage(): String = { return "Export model to file." } }
  var exportToTable = new JMenuItem(
    new AbstractAction("Export to table") {      
      def actionPerformed(event: ActionEvent) =  saveAsTable
    }
  )
  def saveAsTable {
    val location = modelTab.getSelectedIndex
		if (location >= 0 && modelTab.getTabCount() > 0) {
			val container = modelContainers(modelTab.getTitleAt(location))
        val fc: JFileChooser = new JFileChooser()
        if (fc.showSaveDialog(main) == JFileChooser.APPROVE_OPTION) {
          val file: File = fc.getSelectedFile()
          val defaultTabSep = ";" //This should be configurable??
          val tabSep: String = {
            val maybeTabSep = JOptionPane.showInputDialog(null, "Tab separator", defaultTabSep)
            if (maybeTabSep!=null && !maybeTabSep.isEmpty) maybeTabSep else defaultTabSep
          }
          container.model.toTable(tabSep).save(file.getAbsolutePath)
        }
    }    
  }
  
  exportMenu.add(exportToTable)
  border.add(exportMenu)
	border.add(about)
	var listView = new ListView()
	east.add(listView)
	east.add(TooltipPanel, BorderLayout.SOUTH)
	modelTab.addMouseListener(new CloseTabListener())
	//	main.add(border, BorderLayout.NORTH)
	setJMenuBar(menuContainer)
	main.add(modelTab, BorderLayout.CENTER)
	main.add(east, BorderLayout.EAST)
	add(main)
	main.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_S, InputEvent.CTRL_MASK),"Save")
	main.getActionMap.put("Save", new AbstractAction() {
		override def actionPerformed(e: ActionEvent) {
			quickSave()
		}
	})
	main.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke("alt LEFT"),"Go back")
	main.getActionMap.put("Go back", new AbstractAction() {
		override def actionPerformed(e: ActionEvent) {
			backButton.doClick()
		}
	})
	main.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke("alt RIGHT"),"Go forward")
	main.getActionMap.put("Go forward", new AbstractAction() {
		override def actionPerformed(e: ActionEvent) {
			forwardButton.doClick()
		}
	})
	
	if (container == null) {
		openModel(modelName, mainView.container).unsaved()
	} else {
		openContainer(container).unsaved()
	}
	setVisible(true)

	/**
	 * openModel adds a given Model to the list of Models and opens a new tab for said model.
	 * @param name as String which is the name the Model should have.
	 * @param contain is the ModelContainer to be used.
	 */
	def openModel(name: String, contain: ModelContainer): ModelContainer = {
		var container = contain.copy()
		container.name = name
		return openContainer(container)
	}

	/**
	 * openContainer adds a given ModelContainer to the tabs without copying it.
	 * @param container is the ModelContainer to be used.
	 */
	def openContainer(container: ModelContainer): ModelContainer = {
		if (container.name == null) {
			container.name = Config.getSettingAfterReload(Config.DEFAULTMODELKEY)
		}
		container.tmpName = getTmpName(container.name, 0)
		container.gui = this
		container.addObserver(listView)
		names.add(container.tmpName)
		modelContainers += (container.tmpName -> container)
		var view: View = mainView.getClass().getConstructor(container.getClass()).newInstance(container).asInstanceOf[View]
		modelTab.addTab(container.tmpName, view)
		
		view.asInstanceOf[JComponent].getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke("control E"),"Add entity")
		view.asInstanceOf[JComponent].getActionMap.put("Add entity", new AbstractAction() {
			override def actionPerformed(e: ActionEvent) {
				if (modelContainers.get(modelTab.getTitleAt(modelTab.getSelectedIndex())).head == container) {
					new AddEntity().doAction(container)
					modelTab.getComponentAt(modelTab.getSelectedIndex()).asInstanceOf[View].updateView
				}
			}
		})		
		listView.updateView(container)
		modelTab.setSelectedIndex(modelTab.indexOfTab(container.tmpName))
		return container
	}

	/**
	 * openModel adds a given Model to the list of Models and opens a new tab for said model.
	 * @param name as String which is the name the Model should have.
	 * @param model is the Model which is to be used.
	 */
	def openModel(name: String, model: Model): ModelContainer = { return openModel(name, new ModelContainer(model)) }

	/**
	 *  loadModel opens a selected File and creates a new model from it
	 */
	def loadModel() {
		val fc: JFileChooser = new JFileChooser(CURRENTDIRECTORY.toString())
		if (fc.showOpenDialog(main) == JFileChooser.APPROVE_OPTION) {
			var file: File = fc.getSelectedFile()
			if (file.getName().contains(".scala")) {
				val fString: String = loadLines(file.getAbsolutePath()).mkString("\n")
				val m: Model = Model.load(file.getAbsolutePath()) //Model.interpret(fString)
				if ((m.toScala.length() > 7 && fString.contains("Model")) || (fString.length == 0 && m.toScala.length == 7) || fString.equals("Model()")) {
					CURRENTDIRECTORY.updateValue(file.getParent())
					openModel(file.getName(), new ModelContainer(file.getName(), file.getName(), file.getAbsolutePath(), this, m, null))
				} else {
					JOptionPane.showMessageDialog(main, "Error: Syntax error in model file.")
				}
			} else {
				JOptionPane.showMessageDialog(main, "Error: File must be of type .scala")
			}
		}
	}

	/** Returns a free tmpName of a given name **/
	def getTmpName(name: String, nbr: Int): String = {
		var tmpName = name
		if (nbr > 0) {
			tmpName += " (" + nbr + ")"
		}
		if (names.contains(tmpName)) {
			getTmpName(name, nbr + 1)
		} else {
			return tmpName
		}
	}
	
	/**
	 * quickSaves saves a selected Model to its current file file, or call saveModel if it doesn't have any file.
	 */
	def quickSave() {
		var location = modelTab.getSelectedIndex
		if (location >= 0 && modelTab.getTabCount() > 0) {
			var container = modelContainers(modelTab.getTitleAt(location))
			if(container.path != null && container.path.length()>2){
				var file: File = new File(container.path)
				container.model.toScala.save(file.getAbsolutePath())
				modelTab.setIconAt(location, null)
			}else{
				saveModel()
			}
		}
	}

	/**
	 * saveModel saves a selected Model to a selected file and updates the title of the model tab.
	 * @return Boolean true if successful, else false.
	 */
	def saveModel(): Boolean = {
		var location = modelTab.getSelectedIndex
		if (location >= 0 && modelTab.getTabCount() > 0) {
			var container = modelContainers(modelTab.getTitleAt(location))
			val fc: JFileChooser = new JFileChooser()
			if (container.path != null) {
				fc.setCurrentDirectory(new File(container.path))
			}
			if (fc.showSaveDialog(main) == JFileChooser.APPROVE_OPTION) {
				var file: File = fc.getSelectedFile()
				container.model.toScala.save(file.getAbsolutePath())
				container.path = file.getAbsolutePath()
				var fileName = file.getName()
				if (!fileName.contains(".scala")) {
					JOptionPane.showMessageDialog(main, "Warning: A file should have .scala as ending! Saved to file \"" + fileName + "\" anyway.")
				} else {
					JOptionPane.showMessageDialog(main, "Saved to file: \"" + fileName + "\" .")
				}
				var tmpName = container.tmpName
				if (container.name != fileName) {
					tmpName = getTmpName(fileName, 0)
					setTempName(container, fileName, tmpName)
				}
				var OLD_VAR = " (old)"
				for (i <- 0 until modelTab.getTabCount()) {
					var cont = modelContainers(modelTab.getTitleAt(i))
					if (cont != container && cont.path == container.path) {
						cont.unsaved()
						if (!cont.tmpName.contains(OLD_VAR)) {
							setTempName(cont, cont.name, getTmpName(cont.name + OLD_VAR, 0))
						}
						if (cont.name == container.name && cont.tmpName == cont.name) {
							setTempName(cont, cont.name, cont.name + OLD_VAR)
						}
					}
				}
				setTempName(container, container.name, container.name)

				modelTab.setIconAt(modelTab.indexOfTab(container.tmpName), null)
				return true
			}
		}
		return false
	}

	/**
	 * Updates the name of a model tab
	 *  @param container of the model shown in tab
	 *  @param fileName of the model shown in tab
	 *  @param tmpName new name for tab
	 */
	def setTempName(container: ModelContainer, fileName: String, tmpName: String) {
		names.remove(container.tmpName)
		names.add(tmpName)
		modelTab.setTitleAt(modelTab.indexOfTab(container.tmpName), tmpName)
		modelContainers = modelContainers.-(container.tmpName)
		modelContainers = modelContainers.+(tmpName -> container)
		container.name = fileName
		container.tmpName = tmpName
	}

	/**
	 * Launches a new GUI with given View!
	 * @param view is the View to be shown.
	 */
	def launch(view: View): GUI = {
		return new GUI(view.container.name, view)
	}

	/**
	 * Close tab
	 * @param view is the View which is to be closed.
	 */
	def closeTab(view: View) {
		modelTab.removeTabAt(modelTab.getSelectedIndex())
		modelContainers = modelContainers.-(view.container.tmpName)
		names.remove(view.container.tmpName)
		if (modelTab.getTabCount() > 0) {
			modelTab.setSelectedIndex(0);
			listView.updateView(modelTab.getComponent(0).asInstanceOf[View].container)
		} else {
			listView.updateView(new ModelContainer(Model()))
		}
	}

	def updateGlassPane() {
		if (glassPane.isVisible) {
			glassPane.tick()
		}
	}

	var activeProgress: Progress = null
	var thread = new Thread() {
		override def run() {
			while (!isInterrupted()) {
				try {
					if (activeProgress != null) {
						activeProgress.setValue()
						updateGlassPane
						activeProgress.repaintProgress
					}
					if (!isVisible()) {
						return
					}
					Thread.sleep(600)
				} catch {
					case e: Exception =>
				}
			}
		}
	}
	thread.start()

	/** Wake busy glasspane **/
	def wake(p: Progress) {
		if (activeProgress == null) {
			activeProgress = p
			setGlassPane(glassPane)
			glassPane.setVisible(true)
			warn.off
			setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR))
		}
	}

	/** Put glasspane to sleep! **/
	def sleep(p: Progress) {
		if (activeProgress == p) {
			activeProgress = null
			setCursor(Cursor.getDefaultCursor())
			warn.on
			glassPane.setVisible(false)
		}
	}

	/** Listener for closing a model tab on right click **/
	class CloseTabListener() extends MouseAdapter {
		override def mouseClicked(e: MouseEvent) {
			val index = modelTab.indexAtLocation(e.getX, e.getY)
			// Is right mouse button clicked at a tab?
			if (SwingUtilities.isMiddleMouseButton(e) && index >= 0) {
				var view = modelTab.getComponentAt(index).asInstanceOf[View]
				// Is tab unsaved?
				if (modelTab.getIconAt(index) != null) {
					val choice = JOptionPane.showConfirmDialog(gui, "The model that you want to close is unsaved. Do you want to save the model before closing it?", "Close tab unsaved. Save?", JOptionPane.YES_NO_CANCEL_OPTION)
					if ((choice == JOptionPane.YES_OPTION && saveModel())||choice == JOptionPane.NO_OPTION) {
						closeTab(view)
					}
				} else if (JOptionPane.showConfirmDialog(gui, "Are you sure you want to close the model?", "Close model?", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
					closeTab(view)
				}
			}
		}
	}

}

/** The Progress trait informs that the class can progress while sleeping. **/
trait Progress {
	var pbar = new JProgressBar()
	def setValue()
	def repaintProgress()
}

/** Creates safely a ReqTGUI.*/
object ReqTGUI {
	def apply() = new ReqTLauncher(Config.getSettingAfterReload(Config.DEFAULTMODELKEY), Model(), null)
	/** Creates a ReqT with the given ModelContainer, used if to directly modify a Model **/
	def apply(container: ModelContainer) = new ReqTLauncher(null, container.model, container)
	/** Creates a ReqT with the given Model, used if not required to edit with reqT in command prompt. **/
	def apply(name: String, model: Model) = new ReqTLauncher(name, model, null)
	/** Creates a ReqT with the given values used if to edit specifics **/
	def apply(name: String, model: Model, container: ModelContainer) = new ReqTLauncher(name, model, container)
} 

/** Creates safely a GUI.*/
object GUI {
	/**
	 * Constructor used by GUILauncher and should be default command prompt use of GUI.
	 * Gives the model the default name Unnamed.
	 * @param view is a View which should be the default View in the GUI.
	 */
	def apply(name: String, view: View) = new GUILauncher(name, view, null)
	/**
	 * Constructor used by GUI and should be default command prompt use of GUI.
	 * Gives the model the default name Unnamed.
	 * @param view is a View which should be the default View in the GUI.
	 */
	def apply(view: View) = new GUILauncher(Config.getSettingAfterReload(Config.DEFAULTMODELKEY), view, null)
	/** Creates a GUI with the given values used if to edit specifics **/
	def apply(modelName: String, view: View, container: ModelContainer) = new GUILauncher(modelName: String, view: View, container: ModelContainer)
} 

/**
 * Advanced GUI with a MainView.
 * @param name is a String of the name of the Model.
 * @param m is the Model which is to be opened.
 */
class ReqTLauncher(name: String, model: Model, container: ModelContainer) {
	
	/** Creates an unnamed Model if there is no pre-made Model. **/
	def this() = this(Config.getSettingAfterReload(Config.DEFAULTMODELKEY), Model(), null)

	/** Creates a ReqT with the given ModelContainer, used if to directly modify a Model **/
	def this(container: ModelContainer) = this(null, container.model, container)

	/** Creates a ReqT with the given Model, used if no nessecity to edit with reqT in command prompt. **/
	def this(name: String, model: Model) = this(name, model, null)
	SwingUtilities.invokeLater(new Runnable() {
        override def run() {
           	try {
				UIManager.setLookAndFeel("com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel");
				UIManager.put("TabbedPane.textIconGap", 5);
				UIManager.put("TabbedPane.selected", GREENCOLOR);
				UIManager.put("TabbedPane.unselectedBackground", GRAYCOLOR);
				UIManager.put("TabbedPane.shadow", ALMOSTBACKGROUND);
				new ReqTGUI(name, model, container)
           	} catch { case e: Exception => e.printStackTrace()}
         }
     })
}

/**
 * Advanced GUI with a MainView.
 * @param name is a String of the name of the Model.
 * @param m is the Model which is to be opened.
 */
class GUILauncher(modelName: String, view: View, container: ModelContainer) {

	/**
	 * Constructor used by GUILauncher and should be default command prompt use of GUI.
	 * Gives the model the default name Unnamed.
	 * @param view is a View which should be the default View in the GUI.
	 */
	def this(view: View) = this(Config.getSettingAfterReload(Config.DEFAULTMODELKEY), view, null)

	/**
	 * Constructor used by GUILauncher and should be default command prompt use of GUI.
	 * Gives the model the default name Unnamed.
	 * @param view is a View which should be the default View in the GUI.
	 */
	def this(name: String, view: View) = this(name, view, null)
	
	SwingUtilities.invokeLater(new Runnable() {
        override def run() {
           	try {
				UIManager.setLookAndFeel("com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel");
				UIManager.put("TabbedPane.textIconGap", 5);
				UIManager.put("TabbedPane.selected", GREENCOLOR);
				UIManager.put("TabbedPane.unselectedBackground", GRAYCOLOR);
				UIManager.put("TabbedPane.shadow", ALMOSTBACKGROUND);
				new GUI(modelName, view, container)
           	} catch { case e: Exception => e.printStackTrace()}
         }
     })
}



/**
 * Advanced GUI with a MainView.
 * @param name is a String of the name of the Model.
 * @param m is the Model which is to be opened.
 */
class ReqTGUI(name: String, model: Model, container: ModelContainer) extends GUI(name, new MainView(model), container) {

	/** Creates an unnamed Model if there is no pre-made Model. **/
	def this() = this(Config.getSettingAfterReload(Config.DEFAULTMODELKEY), Model(), null)

	/** Creates a ReqTGUI with the given ModelContainer, used if to directly modify a Model **/
	def this(container: ModelContainer) = this(null, container.model, container)

	/** Creates a ReqTGUI with the given Model, used if no nessecity to edit with reqT in command prompt. **/
	def this(name: String, model: Model) = this(name, model, null)
	setTitle("ReqTGUI")

	/** Overrides the creation of a new GUI. Instead searches and loads given view. **/
	override def launch(view: View): GUI = {
		var v = modelTab.getComponentAt(modelTab.indexOfTab(view.container.tmpName)).asInstanceOf[MainView].search(view.toString()).asInstanceOf[View].updateView()
		return this;
	}

}

/** Standard busy GlassPane **/
class GlassPane extends JPanel {
	var STATUS = 0
	var MAX = 10
	setOpaque(false)
	addMouseListener(new MouseAdapter() {})
	addMouseMotionListener(new MouseMotionAdapter() {})
	addKeyListener(new KeyAdapter() {})
	override def paintComponent(g: java.awt.Graphics) {
		try {
			super.paintComponent(g)
		} catch {
			case e: Exception =>
		}
		g.setColor(new Color(10, 10, 15, 20))
		g.fillRect(0, 0, getWidth, getHeight)
		g.setColor(new Color(250, 250, 255, 40))
//		g.fillRect(STATUS * getWidth() / MAX, 0, getWidth() / MAX, getHeight)
	}

	/** Tick visual view **/
	def tick() {
		STATUS += 1
		STATUS %= MAX
		updateUI()
	}
}
