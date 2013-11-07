package gui

import java.awt.BorderLayout
import java.awt.Component
import java.awt.Dimension
import java.awt.GridLayout
import java.awt.event._
import java.util.Arrays
import java.util.Observable
import java.util.Observer
import scala.collection.JavaConverters._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import javax.swing.JButton
import javax.swing.JComboBox
import javax.swing.JLabel
import javax.swing.JList
import javax.swing.JOptionPane
import javax.swing.JPanel
import javax.swing.JScrollPane
import javax.swing.JTabbedPane
import javax.swing.JTextArea
import javax.swing.JTextField
import javax.swing.SwingUtilities
import javax.swing.UIManager
import javax.swing.event._
import reqt._
import javax.swing.SwingConstants
import reqt.Requirement
import java.awt.Color
import reqt.Requirement
import javax.swing.JCheckBox
import java.awt.event.ItemListener
import javax.swing.border.TitledBorder
import java.awt.Point
import java.awt.Cursor
import scala.compat.Platform
import scala.collection.mutable.Stack
import javax.swing.JComponent
import javax.swing.AbstractAction
import javax.swing.KeyStroke
import javax.swing.BorderFactory
import java.awt.BorderLayout
import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics
import java.awt.GridLayout
import java.awt.KeyboardFocusManager
import java.awt.Point
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.event.KeyEvent
import java.util.ArrayList
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.asScalaSet
import javax.swing.BorderFactory
import javax.swing.JButton
import javax.swing.JComboBox
import javax.swing.JLabel
import javax.swing.JList
import javax.swing.JPanel
import javax.swing.SwingConstants
import javax.swing.SwingUtilities
import javax.swing.border.TitledBorder
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.event.ListSelectionEvent
import javax.swing.event.ListSelectionListener
import reqt.Capacity
import reqt.Constr
import reqt.Entity
import reqt.Model
import reqt.Prio
import reqt.Relation
import reqt.Requirement
import reqt.Result
import reqt.Stakeholder
import reqt.Subdomain
import reqt.Submodel
import reqt.warn
import java.awt.Component


/** JTabbedPane with extended functionality for finding children**/
class ReqTabbedPane extends JTabbedPane {

	/**
	 * search finds child which contains given title
	 * @param String is the text which is searched.
	 * @return Component that has searched tab title.
	 */
	def search(s: String)(implicit m: ClassManifest[ReqTabbedPane]): Component = {
		var index: Int = this.indexOfTab(s)
		if (index == (-1)) {
			for (i <- 0 until getTabCount()) {
				var response: Component = null
				var c: ReqTabbedPane = null
				try {
					c = m.erasure.cast(getComponentAt(i)).asInstanceOf[ReqTabbedPane]
				} catch {
					case e: Throwable =>
				}
				if (c != null) {
					response = c.search(s)
					if (response != null) {
						setSelectedIndex(i)
						return response
					}
				}
			}
			return null
		} else {
			setSelectedIndex(index)
			return getComponentAt(index)
		}
	}

	/**
	 * addTab with label if to personalize the tab.
	 * @param label is the label which is to be seen as the tab. Needs to have a title as text!
	 * @param view is the view to be added.
	 */
	def addTab(label: JLabel, v: View) {
		addTab(label.getText(), v)
		setTabComponentAt(indexOfTab(label.getText()), label)
	}

}

/** A View can be viewed by GUI  **/
trait View extends Component {
	var container: ModelContainer = _
	addComponentListener(new ComponentAdapter() {
		/** Update if the View is resized **/
		override def componentResized(e: ComponentEvent) {
			if (isVisible()) {
				updateView()
			}
		}
		/** Update if the View becomes visible **/
		override def componentShown(e: ComponentEvent) {
			try{
				updateView()
			}catch{
				case e : Exception => warn(e.getMessage())
			}
		}
	})
	/** updateView updates the graphical view of the View **/
	def updateView()

	/** Launch a specific view
	 * @param v is the View which is to be launched. 
	 **/
	def launch(v: View) {
		container.launch(v)
	}
}

/**
 * A general View which should contain tabs.
 * @param contain The ModelContainer to be used.
 */
abstract class TabbedView(contain: ModelContainer) extends ReqTabbedPane with View {
	this.container = contain
	addMouseListener(new MouseAdapter(){
		override def mousePressed(ev: MouseEvent) {
			val point = ev.getPoint()
			val index = indexAtLocation(point.x, point.y)
			if (index > -1) {
				if (getComponentAt(index).isInstanceOf[View]) {
					container.addShowTab((getComponentAt(index).asInstanceOf[View], container.entity))
				}
			}
		}
	})
	
	/**
	 * Used by command prompt to create a GUI with the TabbedView.
	 * @param model The reqT model to be used.
	 */
	def this(model: Model) = this(new ModelContainer(model))

	/**
	 * Used by command prompt to create a GUI with the TabbedView.
	 * Lacks model so requires the user to open or create a new model.
	 */
	def this() = this(Model())

	/**
	 * Add tab to a tabbedView and give said tab the TabbedView's launcher.
	 * @param title of the tab as a String.
	 * @param view The View to be added as a tab.
	 */
	def addTab(title: String, view: View) {
		view.container = container
		super.addTab(title, view)
	}

	/**
	 * Add tab to a tabbedView and give said tab the TabbedView's launcher.
	 * Sets title to toString of the view.
	 * @param view The View to be added as a tab.
	 */
	def addTab(view: View) {
		view.container = container
		super.addTab(view.toString(), view)
	}

}

/**
 * A general View which is scrollable.
 * @param contain The ModelContainer to be used.
 */
abstract class PanelView(contain: ModelContainer) extends JScrollPane with View {
	this.container = contain
	getVerticalScrollBar().setUnitIncrement(40)
	/**
	 * Used by command prompt to create a GUI with the PanelView viewing given entity.
	 * @param model The reqT model to be used.
	 * @param entity The reqT Entity to be viewed.
	 */
	def this(model: Model, entity: Entity) = this(new ModelContainer(model, entity))

	/**
	 * Used by command prompt to create a GUI with the PanelView without viewing any Entity.
	 * @param model The reqT model to be used.
	 */
	def this(model: Model) = this(new ModelContainer(model))

	/**
	 * Used by command prompt to create a GUI with the PanelView without any model.
	 * Lacks model so requires the user to open or create a new model.
	 */
	def this() = this(new ModelContainer(Model()))

	/**
	 * Sets the view of the Viewport.
	 * @param c is the Component to be set as View to the Viewport.
	 */
	def setView(c: Component) {
		this.getViewport().setView(c)
	}

	/** Revalidates Viewport **/
	def revalidateView() {
		try {
			this.getViewport().getView().asInstanceOf[JComponent].setBorder(BorderFactory.createEmptyBorder(5, 10, 5, 10))

			this.getViewport().revalidate()
			updateUI()
		} catch {
			case e: Exception =>
		}
	}

}

/** ListView is the view of all entities of a Model. */
class ListView() extends JPanel with Observer with WITHGREENCOLOR{

	var DEFAULTALL = "All"
	var DEFAULTFILTER = "Filter"

	var listSize = new Dimension(200, 200)
	setSize(listSize)
	setPreferredSize(listSize)
	setLayout(new BorderLayout())
	var modelList = new JList[EntityContainer] with Tooltip {
		override def getMessage(): String = { return "Select Entity to view detailed information." }
	}
	modelList.addListSelectionListener(new ListSelectionListener() {
		/** User selects an Entity which causes a detailed view of the Entity to be shown. **/
		override def valueChanged(e: ListSelectionEvent) {
			var e = modelList.getSelectedValue()
			if (e != null) {
				container.entity = e.entity
				container.launch(new EntityView(container))
			}
		}
	})
	modelList.setBackground(ALMOSTBACKGROUND)
	var north = new JPanel(new GridLayout(3, 1)) with WITHGREENCOLOR
	var scroll = new JScrollPane(modelList)
	var allTypes = reqt.entityKinds.map(b => b.toString()) :+ DEFAULTALL
	var entityType = new JComboBox[String](reqt.entityKinds.map(b => b.toString()).sorted.toArray) with Tooltip {
		override def getMessage(): String = { return "Select Entity type to add." }
	}
	entityType.setSelectedItem("Feature")
	allTypes = allTypes.sorted

	var types = new JComboBox[String](allTypes.toArray) with Tooltip {
		override def getMessage(): String = { return "Select Entity type to filter model with." }
	}
	types.setSelectedItem(DEFAULTALL)
	types.addActionListener(new ActionListener() {
		/** User selects to filter entities by a type. **/
		override def actionPerformed(e: ActionEvent) {
			filter.setText(DEFAULTFILTER)
			setTypeFilter()
			listFilter()
		}
	})
	var filter = new TooltipField(DEFAULTFILTER, "Enter text to filter model with.")
	filter.getDocument().addDocumentListener(new DocumentListener() {
		/** User edits filter. **/
		override def changedUpdate(e: DocumentEvent) { listFilter() }
		override def removeUpdate(e: DocumentEvent) { listFilter() }
		override def insertUpdate(e: DocumentEvent) { listFilter() }
	})
	var entities: List[EntityContainer] = List()
	var container: ModelContainer = _
	var undoButton = new JButton() with Tooltip {
		enable(false)
		addActionListener(new ActionListener() {
			/** User presses undo **/
			override def actionPerformed(e: ActionEvent) {
				try {
					container.history.undo()
					updateView(container)
					container.gui.modelTab.getSelectedComponent().asInstanceOf[View].updateView
				} catch {case e: Exception =>}
			}
		})
		override def getMessage(): String = getText()
	}
	add(scroll, BorderLayout.CENTER)
	add(north, BorderLayout.NORTH)
	var southPanel = new JPanel(new GridLayout(1, 2)) with WITHGREENCOLOR
	southPanel.add(entityType)
	southPanel.add(new ActionButton("Add",new ActionListener() {
		/** Delete selected entity **/
		override def actionPerformed(e: ActionEvent) {
			var value = entityType.getSelectedItem()
			if(value!=null){
				(new AddEntity()).doAction(container, value.toString())
				container.gui.modelTab.getSelectedComponent().asInstanceOf[View].updateView
			}
		}
	}))
	add(southPanel, BorderLayout.SOUTH)
	north.add(undoButton)
	north.add(types)
	north.add(filter)

	/** If field is not empty nor contains "Filter" it filters the list by ID. Updates list. **/
	def listFilter() {
		var newEntities: List[EntityContainer] = List[EntityContainer](entities.toArray: _*)
		if (filter.getText() != DEFAULTFILTER && (!filter.getText().trim().isEmpty())) {
			var text = filter.getText().toLowerCase()
			newEntities = entities.filter(e => e.entity.id.toLowerCase() == text || e.entity.id.toLowerCase().contains(text))
		}
		modelList.setListData(newEntities.toArray)
		scroll.getViewport().revalidate()
	}

	/** Filters list by type, does not update list. **/
	def setTypeFilter() {
		entities = for (e <- container.model.entities.toList) yield (new EntityContainer(e))
		if (types.getSelectedItem().toString() != DEFAULTALL) {
			entities = entities.filter(e => e.entity.prefix == types.getSelectedItem().toString())
		}
		entities = entities.sortWith((new SortFactory(container)).compareEntityId)
	}

	/**
	 * Updates view
	 * 	Updates undoButton to last action
	 */
	def updateView(contain: ModelContainer) {
		container = contain
		val action = container.history.thisAction
		if (action == "Undo") {
			undoButton.setText("Redo: " + container.history.prevAction)
		} else {
			undoButton.setText("Undo: " + container.history.thisAction)
		}
		if (undoButton.getText() != "Undo: ") {
			undoButton.enable(true)
		}
		setTypeFilter()
		listFilter()
	}

	/** Updates view in case modelContainer has changed. **/
	override def update(modelContainer: Observable, o: Object) {
		updateView(modelContainer.asInstanceOf[ModelContainer])
	}

}

/** ReqTGui main container which connects with complete functionality**/
class MainView(contain: ModelContainer) extends TabbedView(contain) {

	def this(m: Model) = this(new ModelContainer(m))

	def this() = this(Model())

	addTab(new ViewsView(contain))
	addTab(new EntityView(contain))
	addTab(new PrioritizationView(contain))
	addTab(new ReleasePlanHolder(contain))
	addTab(new ExtractionView(contain))

	def updateView() {
		for (c <- getComponents) { //TODO: Loop through tabs
			c.asInstanceOf[View].updateView()
		}
	}
}

/** Holder of the ReleasePlan editing and board **/
class ReleasePlanHolder(contain: ModelContainer) extends TabbedView(contain) {
	addTab(new ReleasePlanView(contain))
	addTab(new ReleaseBoard(contain))
	
	def updateView() {
		for (c <- getComponents) { //TODO: Loop through tabs
			c.asInstanceOf[View].updateView()
		}
	}
	
	override def toString() : String = {return "Plan"}
	
}


/** View of Details**/
/*class DetailsView(contain: ModelContainer) extends TabbedView(contain) {
	var entities = new EntityView(contain)
	var label = new JLabel("Select type and name of the entity.") with WITHGRAYCOLOR
	label.setVerticalAlignment(SwingConstants.TOP)
	label.setOpaque(true)
	addTab(entities)
	// Removed as Add button added at List.
	/*addTab("Add new entity", label)
	addMouseListener(new MouseAdapter() {
		override def mousePressed(e: MouseEvent) {
			if (getTitleAt(getSelectedIndex()).contains("Add")) {
				(new AddEntity()).doAction(container)
			}
			setSelectedIndex(0)
		}
	})*/
	def updateView() {
		entities.updateView()
	}
	override def toString() = "Add / Edit"
}*/

class AddEntity() {

	def doAction(container: ModelContainer): Entity = {
		var array = AllEntities.keySet.toArray[Object]
		Arrays.sort(array)
		var entityType = JOptionPane.showInputDialog(container.gui, "Which entity type do you wish to create?", "Input", JOptionPane.INFORMATION_MESSAGE, null, array, null)
		if (entityType != null && entityType.toString.size > 1) {
			return doAction(container, entityType.toString)
		}
		return null
	}

	def doAction(container: ModelContainer, entityType: String): Entity = {
		var entityName: Object = ""
		while (entityName != null && entityName.toString.trim.isEmpty) {
			entityName = JOptionPane.showInputDialog(container.gui, "What is the name of the new " + entityType + "?", "Input", JOptionPane.INFORMATION_MESSAGE, null, null, null)
		}
		if (entityName != null) {
			if (container.model.ids.contains(entityName)) {
				JOptionPane.showMessageDialog(container.gui, "Error: ID (" + entityName + ") is not unique. Entity was not added.", "ID must be unique", JOptionPane.OK_OPTION)
			} else {
				container.entity = container.addEntity(entityType, entityName.toString)
				return container.entity
			}
		}
		return null
	}
}

/** Default PanelViews are below **/

/** View for showing and editing an entity **/
class EntityView(contain: ModelContainer) extends PanelView(contain) {
	import scala.collection.mutable.Set
	import scala.collection.JavaConverters._
	import javax.swing._
	import scala.collection.mutable.HashMap

	def this(model: Model) = this(new ModelContainer(model))
	def this() = this(Model())
	def this(model: Model, entity: Entity) = this(new ModelContainer(model, entity))

	var main = new JPanel(new BorderLayout()) with WITHGRAYCOLOR
	var panel = new StructurePanel(this)
	val image = new ImageLabel("")
	var deleteButton = new ActionButton("Delete",new ActionListener() {
		/** Delete selected entity **/
		override def actionPerformed(e: ActionEvent) {
			var answer = JOptionPane.showConfirmDialog(container.gui, "Are you sure you want to delete " + container.entity + "?", "Are you sure?", JOptionPane.YES_NO_OPTION);
			if (answer == JOptionPane.YES_OPTION) {
				deleteSelectedEntity()
			}
		}
	})

	main.add(panel, BorderLayout.CENTER)
	this.setView(main)

	val IMAGELABEL = "Image"

	var relations = reqt.edgeKinds.filter(_.isInstanceOf[Relation]).map(b => b.toString()).sortWith(_.compareTo(_) < 0).asJava.toArray()
	var reqBox = new JComboBox(reqt.requirementKinds.asJava.toArray())

	var entities = container.model.entities.map(b => b.toScala).toList

	val attrMap = new HashMap[String, AttributeCoupledExtended]()
	val relMap = new HashMap[JComboBox[Object], JComboBox[Object]]()
	val costMap = new HashMap[String, CostCoupledExtended]()
	
	var resourceOrCost = ""
	var showReleaseNbr = false

	var addAttrButton: JButton = _ // TEST
	var addRelButton: JButton = _ // TEST
	var addCostButton: JButton = _ // TEST
	var addResourceButton: JButton = _ // TEST
	var entityBox: JComboBox[Object] = _ // TEST
	var relationBox: JComboBox[Object] = _ // TEST
	var attributeBox: JComboBox[Object] = _ // TEST
	var costBox: JComboBox[Object] = _ // TEST
	var costField: NumberField = _ // TEST
	var entityIdButton: TextButton = _ // TEST
	var releaseNbrButton: TextButton = _ // TEST
	var DEFAULTSIZE = 350

	def deleteSelectedEntity() {
		container.removeEntity(container.entity)
		updateView()
	}

	/** updateView resets view when called. **/
	override def updateView() {
		panel.clean()
		if(container.gui!=null){
			DEFAULTSIZE = 350 max (3*container.gui.getWidth/5)
		}
		image.setIcon(null)
		if (!container.entity.isInstanceOf[reqt.Requirement]) {
			resourceOrCost = "capacity"
			showReleaseNbr = false
		} else {
			resourceOrCost = "cost"
			showReleaseNbr = true
		}
		panel.addSeparator()
		if (container.entity != null && container.model.entities.contains(container.entity)) {
			panel.addTitle(container.entity.prefix)
			panel.addSeparator()
			if (container.entity != null) {
				var typePanel = new JPanel(new BorderLayout())  with WITHGRAYCOLOR
				var typeContainer = new JPanel(new GridLayout(2,2))  with WITHGRAYCOLOR
				typePanel.add(typeContainer, BorderLayout.CENTER)
				typePanel.add(image, BorderLayout.EAST)
				entityIdButton = new TextButton(container.entity.id, new IdAction())
				typeContainer.add(new ReqLabel("Type: "))
				typeContainer.add(new TextButton(container.entity.prefix, new ActionListener(){
					override def actionPerformed(e : ActionEvent){
						var array = AllEntities.keySet.toArray[Object]
						Arrays.sort(array)
						var entityType = JOptionPane.showInputDialog(container.gui, "Into what entity type do you want to transform "+container.entity.prefix+" "+container.entity.id+"?", "Input", JOptionPane.INFORMATION_MESSAGE, null, array, null)
						if (entityType != null && entityType.toString.size > 1) {
							container.updateEntity(reqt.entityFromString(entityType.toString)(container.entity.id))
							updateView()
						}
					}
				}))
				typeContainer.add(new ReqLabel("ID: "))
				typeContainer.add(entityIdButton)
				panel.addWest(typePanel, 350)
				if (showReleaseNbr) {
					(container.model / Release).entities.foreach { rel =>
						if (((container.model / rel !! Submodel) / container.entity).entities.size > 0) {
							releaseNbrButton = new TextButton(rel.toString, new ActionListener() {
								override def actionPerformed(e: ActionEvent) {
									container.launch(new ReleaseBoard(container))
								}
							})
							panel.addDoubleGridWest(new ReqLabel("Released in:"), releaseNbrButton, 350)
						}
					}
				}
				panel.addSeparator()
				panel.addDoubleWest(new ActionButton("View", new ActionListener() {
					override def actionPerformed(a: ActionEvent) {
						container.launch(new NodeHolder(container))
					}
				}), deleteButton ,350)
				panel.addWest(new JLabel(" "), 500)
				panel.addSeparator()
				panel.addTitle("Attributes")
				panel.addSeparator()
				showAttributes()
				panel.addSeparator()
				panel.addSeparator()
				panel.addTitle("Relations")
				panel.addSeparator()
				showRelations()
				panel.addSeparator()
				panel.addSeparator()
				panel.addTitle("Textual")
				panel.addSeparator()
				var scalaSize = new Dimension(DEFAULTSIZE, 250)
				var scala = new ScalaArea(container){
					override def getText() : String = {
						return (container.model / container.entity).toScala
					}
					override def updateView(){
						if(isSelected()){
							write()
							revalidateView()
						}
					}
					override def toString() : String = {
						return "EntityView Scala"
					}
				}
				scala.setSize(scalaSize)
				scala.setPreferredSize(scalaSize)
				scala.updateView
				panel.addWest(scala)
				/*var deletePanel = new JPanel(new BorderLayout()) with WITHGRAYCOLOR
				var deleteCorner = new JPanel(new BorderLayout()) with WITHGRAYCOLOR
				deleteCorner.add(new TitleLabel("Delete " + container.entity.id + ": "), BorderLayout.CENTER)
				deleteCorner.add(deleteButton, BorderLayout.EAST)
				deletePanel.add(deleteCorner, BorderLayout.WEST)
				east.add(deletePanel, BorderLayout.NORTH)*/
				if (container.entity.isInstanceOf[reqt.Requirement]) {
					panel.addSeparator()
					panel.addSeparator()
					panel.addTitle("Priorities")
					panel.addWest(new ReqPrioPanel(container))
				}
			}
		} else {
			panel.add(new ReqLabel("Select an entity from the list or add a new entity with the \"Add\" button below the list to the right."))

		}
		panel.resized()
		revalidateView()
	}
	def isSelected() : Boolean = {
		return container.tempTab!=null&&(container.tempTab._1.toString==toString||container.tempTab._1.toString==this.getParent().toString)
	}

	/** updates the list of attributes shown**/
	def showAttributes() {
		attrMap.clear
		var attr = container.model.attributesOf(container.entity) //.toList.sortWith((a, b)=>a.toString().compareTo(b.toString() ) < 0)
		attr.foreach { a =>
			/** Add row for each attribute. **/
			if(a.isInstanceOf[LevelValue]){
				var strings = for(lvl <- reqt.levelVector) yield (lvl.toString)
				var combo = new JComboBox[String](strings.toArray)
				var value = ((container.model/container.entity)!!Status).toString
				combo.setSelectedItem(value)
				var attribute = new AttributeCombo(a.prefix, combo)
				if (a.prefix == IMAGELABEL) {
					image.setImage(a.value.toString)
				}
				panel.addWest(attribute, DEFAULTSIZE)
			}else{
				var f: javax.swing.text.JTextComponent = new JTextField(a.value.toString) with SelectableField
				var comp : JComponent = f
				if(Config.getSettingAfterReload(a.prefix)==Config.DEFAULTAREAVALUE){
					var scroll = new JScrollPane
					f = new JTextArea(a.value.toString) with SelectableField{
						setLineWrap(true)
						setWrapStyleWord(true)
					}
					var d = new Dimension(200,100)
					scroll.setSize(d)
					scroll.setPreferredSize(d)
					scroll.getViewport().setView(f)
					comp = scroll
					}else if(Config.getSettingAfterReload(a.prefix)==Config.DEFAULTCONSTRAINTVALUE){
						var scroll = new JScrollPane
						f = new JTextArea(a.value.toString) with SelectableField{
							setLineWrap(true)
							setWrapStyleWord(true)
							setEditable(false)
							addMouseListener(new MouseAdapter(){
								override def mouseClicked(e : MouseEvent){
									JOptionPane.showMessageDialog(container.gui, new ConstraintText(container, container.entity));
									updateView()
								}
							})
						}
						var d = new Dimension(200,100)
						scroll.setSize(d)
						scroll.setPreferredSize(d)
						scroll.getViewport().setView(f)
						comp = scroll
					}else if (a.isInstanceOf[IntValue]) {
						f = new NumberField(a.value.toString, "Enter numerical value")
						comp = f
					}
					var attribute = new AttributeCoupledExtended(a.prefix, f, comp)
					if (a.prefix == IMAGELABEL) {
						image.setImage(a.value.toString)
					}
					panel.addWest(attribute, DEFAULTSIZE)
					attrMap.put(a.prefix, attribute)
			}
		}
		attributeBox = new JComboBox(AllAttributes.keySet.--(attr.map(b => b.prefix).toSet).toSeq.sortWith(_.compareTo(_) < 0).asJava.toArray());
		attributeBox.setSelectedItem(null)
		attributeBox.setPreferredSize(new Dimension(100, 20))
		//attributeField.setPreferredSize(new Dimension(200, 20))
		addAttrButton = new ActionButton("Add attribute", new AddAttributeAction())
		panel.addSeparator()
		panel.addWest(new ReqLabel("Select attribute kind and click add attribute."), 350)
		panel.addDoubleWest(attributeBox, addAttrButton, 350)
	}
	
	class ConstraintText(container : ModelContainer, entity : Entity) extends JPanel{
		setLayout(new BorderLayout())
		add(new TitleLabel(entity.id), BorderLayout.NORTH)
		var textarea = new JTextArea("Constraints("+((container.model/entity)!!Constraints).toScala+")")
		var scroll = new JScrollPane(textarea)
		var d = new Dimension(500, 500)
		scroll.setSize(d)
		scroll.setPreferredSize(d)
		add(scroll, BorderLayout.CENTER)
		
		var save = new JButton("Save")
		save.addActionListener(new ActionListener(){
			override def actionPerformed(e : ActionEvent){
				var before = ((container.model/entity)!!Constraints).toScala
				container.has(entity, AllAttributes("Constraints"), textarea.getText())
				var after = ((container.model/entity)!!Constraints).toScala
				if(before==after){
					 scala.swing.Dialog.showMessage(null, "Exception when adding Constraints - no changes were made, If intention was to edit Constraints, please check spelling", "No changes were made.")
				}else{
					 scala.swing.Dialog.showMessage(null, "Saved: "+after, "Saved constraints")
				}
			}
		})
		add(save, BorderLayout.SOUTH)
	}

	class AddAttributeAction extends ActionListener {
		/** Add attribute if added. **/
		override def actionPerformed(e: ActionEvent) {
			if (attributeBox.getSelectedItem() != null) {
				var e = attributeBox.getSelectedItem().toString
				var attr = AllAttributes(e)
				var default = attr.asInstanceOf[Attribute[Any]].default
				try {
					container.has(e, container.entity, default.toString)
				} catch {
					case e: Exception => scala.swing.Dialog.showMessage(null, "Exception when adding " + attributeBox.getSelectedItem().toString() + " with invalid value " + default.toString + ".", "Invalid value.")
				}
				updateView()
			}
		}
	}

	/** updates the list of relations shown **/
	def showRelations() {
		relMap.clear
		var entities = container.model.entities.toList.sortBy(_.prefix)
		var rel = (container.model / container.entity).collect { case (Key(e, r), ns) if !(r <==> has) => (r.prefix, ns.nodes) }
		rel.keySet.foreach { relation =>
			rel(relation).foreach { entity =>
				var relationPanel = new RelationCoupledExtended(relation, entity.asInstanceOf[Entity], entities, relations)
				panel.addWest(relationPanel)
				relMap.put(relationPanel.relations, relationPanel.entities)
			}
		}
		relationBox = new JComboBox(relations)
		relationBox.setSelectedItem(null)
		relationBox.setPreferredSize(new Dimension(100, 20))
		entityBox = new JComboBox(entities.asJava.toArray())
		entityBox.setSelectedItem(null)
		entityBox.setPreferredSize(new Dimension(200, 20))
		addRelButton = new ActionButton("Add relation", new ActionListener() {
			def actionPerformed(e: ActionEvent) {
				if (relationBox.getSelectedItem() != null && entityBox.getSelectedItem() != null) {
					container.relation(relationBox.getSelectedItem().toString(), container.entity, entityBox.getSelectedItem().asInstanceOf[Entity])
					updateView()
				}
			}
		})
		addRelButton.setPreferredSize(addAttrButton.getPreferredSize())
		panel.addSeparator()
		panel.addWest(new ReqLabel("Select relation kind, relation destination and click add relation."), 350)
		panel.addWest(new SimpleBorderPanel(relationBox, entityBox, addRelButton, 350))
	}

	override def toString() = "Update"

	/**
	 * A table of all stakeholders priorities for the selected entity in the ModelContainer
	 * @param container represents the model
	 */
	class ReqPrioPanel(container: ModelContainer) extends JPanel(new BorderLayout())  with WITHGRAYCOLOR{

		var methodSet: Set[Entity] = new scala.collection.mutable.HashSet[Entity]()
		for (s <- (container.model / has).entitiesOfKind(Stakeholder)) {
			((((container.model / s) !! Submodel) / Subdomain("Prio")) !! Submodel).entities.foreach { method => methodSet += method }
		}
		var methods: List[Entity] = methodSet.toList.sortBy(_.id)
		if (methods.size > 0) {
			var prioMethods = new JPanel() with WITHGRAYCOLOR
			prioMethods.setBorder(BorderFactory.createEmptyBorder())
			prioMethods.add(new JLabel(""))
			methods.foreach { m => prioMethods.add(new JLabel(m.id + "  ", SwingConstants.RIGHT)) }
			prioMethods.setBorder(BorderFactory.createMatteBorder(0, 0, 0, 1, Color.BLACK))
			prioMethods.add(new JLabel(""))
			prioMethods.setLayout(new GridLayout(prioMethods.getComponentCount(), 1))
			prioMethods.setPreferredSize(new Dimension(100, 30 * (methods.size)))
			var stakeholders = (container.model / Submodel).entitiesOfKind(Stakeholder)
			var shPrio = new JPanel() with WITHGRAYCOLOR
			stakeholders.foreach { s =>
				var stakeholder = new JLabel(s.id, SwingConstants.CENTER);
				stakeholder.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.BLACK));
				shPrio.add(stakeholder)
			}
			methods.foreach { domain =>
				stakeholders.foreach { s =>
					shPrio.add(new JLabel("" + ((((container.model / s !! Submodel) / Subdomain("Prio") !! Submodel) / domain !! Submodel) / container.entity / has !! Prio), SwingConstants.CENTER))
				}
			}
			shPrio.setLayout(new GridLayout(methods.size + 1, stakeholders.size))
			var scroll = new JScrollPane(shPrio)
			scroll.setBorder(BorderFactory.createEmptyBorder())
			scroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS)
			scroll.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER)
			scroll.setPreferredSize(new Dimension(200 + (new JButton("Add attribute")).getPreferredSize().getWidth().toInt, 10 * (methods.size + 1)))
			scroll.getViewport.revalidate()
			setPreferredSize(new Dimension(200 + (new JButton("Add attribute")).getPreferredSize().getWidth().toInt + 100, 30 * (methods.size) + 20))
			add(prioMethods, BorderLayout.CENTER)
			add(scroll, BorderLayout.EAST)
		}
	}

	/**
	 * ActionListener for updating EntityID
	 * Prompts the user for new name, prompts if invalid
	 */
	class IdAction() extends ActionListener {
		def actionPerformed(e: ActionEvent) {
			var newId = JOptionPane.showInputDialog(container.gui, "Enter new ID for " + container.entity)
			if (newId != null) {
				if (!container.updateEntity(reqt.entityFromString(container.entity.prefix)(newId.trim()))) {
					JOptionPane.showMessageDialog(container.gui, "Error: new ID (" + newId + ") is not unique. Name change was not performed.", "ID must be unique", JOptionPane.OK_OPTION)
				} else {
					updateView()
				}
			}
		}
	}

	/**
	 * ActionListener for deleting an attribute
	 * @param attr The name of the attribute to be deleted
	 */
	class XAttrAction(val attribute: String) extends ActionListener {
		def actionPerformed(e: ActionEvent) {
			container.removeHas(attribute, container.entity)
			updateView()
		}
	}

	/**
	 * ActionListener for deleting a resource
	 * @param resource The name of the resource to be deleted
	 */
	class XDelResAction(val resource: String) extends ActionListener {
		def actionPerformed(e: ActionEvent) {
			if (JOptionPane.showConfirmDialog(container.gui, "If you delete this resource it will be removed for the entire model. Are you sure?") == JOptionPane.OK_OPTION) {
				container.removeResource(resource)
				updateView()
			} else {

			}
		}
	}

	/**
	 * ActionListener for editing a resource
	 * @param resource The name of the resource to be edited
	 */
	class EditResAction(val resource: String) extends ActionListener {
		def actionPerformed(e: ActionEvent) {
			var newName = JOptionPane.showInputDialog(container.gui, "Enter new name of resource " + resource + "\n This will change the name for all requirements");
			if (newName != null) {
				if (!container.renameResource(resource, newName)) {
					JOptionPane.showMessageDialog(container.gui, "There already is a resource called " + newName)
				} else {
					updateView()
				}
			}
		}
	}

	/**
	 * ActionListener for deleting a relation
	 * @param box The JComboBox containing the relations that can be selected to be deleted of given relation.
	 * @param entBox The JComboBox containing the entities that can be selected to be deleted of given relation.
	 */
	class XRelAction(val box: JComboBox[Object], val entBox: JComboBox[Object]) extends ActionListener {
		def actionPerformed(e: ActionEvent) {
			container.removeRelation(box.getSelectedItem().toString(), container.entity, entBox.getSelectedItem().asInstanceOf[Entity])
			updateView()
		}
	}

	class AttributeCombo(var attr : String, var combo : JComboBox[String]) extends BorderPanel(new JLabel(attr.toString), new Dimension(100, 20), combo, null, new XButton(new XAttrAction(attr.toString)), ButtonSize, DEFAULTSIZE) {
		combo.addActionListener(new ActionListener(){
			override def actionPerformed(e : ActionEvent){
				var s = combo.getSelectedItem()
				if(s!=null){
					container.has(container.entity, AllAttributes(attr), s.toString)
				}
			}
		})
	}

	
	/** AttributeCoupled is used by show attributes by creating a BorderPanel with given components **/
	class AttributeCoupled(var label: JLabel, var c: JComponent, var button: XButton) extends BorderPanel(label, new Dimension(100, 20), c, null, button, ButtonSize, DEFAULTSIZE) {}
	/** AttributeCoupledExtended extends AttributeCoupled and prepares the components to be used for editing the given attribute **/
	class AttributeCoupledExtended(var attribute: String, var field: javax.swing.text.JTextComponent, s : JComponent) extends AttributeCoupled(new JLabel(attribute.toString), s, new XButton(new XAttrAction(attribute.toString))) {
		var preferred = field.getPreferredSize()
		
		field.getDocument().addDocumentListener(new DocumentListener() {
			/** User edits attribute. **/
			override def changedUpdate(e: DocumentEvent) { editAttribute() }
			override def removeUpdate(e: DocumentEvent) { editAttribute() }
			override def insertUpdate(e: DocumentEvent) { editAttribute() }
		})
		/** editAttribute executes an editing of the attribute for the given attribute **/
		def editAttribute() {
			var text = field.getText()
			if (attribute == IMAGELABEL) {
				text = text.replace("\\", "/")
				image.setImage(text)
				if (image.getIcon() == null || image.getSize().getWidth() < 1) {
					field.setBackground(LIGHTWARNINGCOLOR)
				} else {
					field.setBackground(LIGHTGOODCOLOR)
				}
			}
			container.has(attribute.toString, container.entity, text)
			field.requestFocus()
		}
	}
	/** CostCoupled is used by show costs by creating a BorderPanel with given components **/
	class CostCoupled(var nameButton: TextButton, var field: NumberField, var button: XButton) extends BorderPanel(nameButton, new Dimension(100, 20), field, new Dimension(200, 20), button, ButtonSize, DEFAULTSIZE) {}
	/** CostCoupledExtended extends CostCoupled and prepares the components to be used for editing the given cost**/
	class CostCoupledExtended(var entity: Entity, var resource: String, var value: Int) extends CostCoupled(new TextButton(resource, new EditResAction(resource)), new NumberField("" + value, "Cost of resource " + resource), new XButton(new XDelResAction(resource))) {
		field.getDocument().addDocumentListener(new DocumentListener() {
			/** User edits cost. **/
			override def changedUpdate(e: DocumentEvent) { editCost() }
			override def removeUpdate(e: DocumentEvent) { editCost() }
			override def insertUpdate(e: DocumentEvent) { editCost() }
		})

		/** editCost executes an editing of the cost for the given Resource **/
		def editCost() {
			if (entity.isInstanceOf[Context]) {
				container.addCapacity(entity, resource, field.getValue)
			} else {
				container.addCost(entity, resource, field.getValue)
			}
			field.requestFocus()
		}
	}
	/** RelationCoupled is used by show relations by creating a BorderPanel with given boxes **/
	class RelationCoupled(var relations: JComboBox[Object], var entities: JComboBox[Object]) extends BorderPanel(relations, new Dimension(100, 20), entities, new Dimension(200, 20), new XButton(new XRelAction(relations, entities)), ButtonSize, DEFAULTSIZE) {}
	/** RelationCoupledExtented extends RelationCoupled and prepares boxes to be used for editing the given relation. **/
	class RelationCoupledExtended(var relation: Object, var entity: Entity, var entitiesList: List[Object], var relationsArray: Array[Object]) extends RelationCoupled(new JComboBox[Object](relationsArray), new JComboBox[Object](entitiesList.asJava.toArray())) {
		relations.setSelectedItem(relation)
		entities.setSelectedItem(entity)
		relations.addActionListener(new ActionListener() { def actionPerformed(e: ActionEvent) { editRelation() } })
		entities.addActionListener(new ActionListener() { def actionPerformed(e: ActionEvent) { editRelation() } })

		/** editRelation executes an editing of the relation for the given relation **/
		def editRelation() {
			if (relations.getSelectedItem() != null && entities.getSelectedItem() != null) {
				container.removeRelation(relation.toString(), container.entity, entity)
				relation = relations.getSelectedItem()
				entity = entities.getSelectedItem().asInstanceOf[Entity]
				container.relation(relation.toString(), container.entity, entity)
			} else {
				warn("RelationCoupledExtended: Both relation and entity needs to be selected if to edit a relation!")
			}
		}
	}
}

/**
 * ViewsView contains an overview of a given model.
 * @param contain The ModelContainer to be used.
 */
class ViewsView(contain: ModelContainer) extends TabbedView(contain) {
	def this() = this(new ModelContainer(Model()))
	def this(model: Model) = this(new ModelContainer(model))
	def this(model: Model, entity: Entity) = this(new ModelContainer(model, entity))

	var holder = new NodeHolder(container)
	var scala = new ScalaArea(container)

	addTab(holder)
	addTab(scala)
	
	override def updateView() {
		holder.updateView()
		scala.updateView()
	}
	override def toString() = "View"
}

/**
 * PrioritizationView contains a prioritization view of a model.
 * @param contain The ModelContainer to be used.
 */
class PrioritizationView(contain: ModelContainer) extends TabbedView(contain) {

	def this() = this(new ModelContainer(Model()))

	def this(model: Model) = this(new ModelContainer(model))

	def this(model: Model, entity: Entity) = this(new ModelContainer(model, entity))

	var panel = new JPanel(new BorderLayout()) with WITHGRAYCOLOR
	var editPrio = new EditPrioritizationView(contain)
	addTab(editPrio)
	var dollar = new DollarView(contain)
	addTab(dollar)
	var ordinal = new OrdinalView(contain)
	addTab(ordinal)
	var ruhe = new RuheView(contain)
	//addTab(ruhe)

	override def updateView() {
		dollar.updateView
		editPrio.updateView
		ordinal.updateView
		ruhe.updateView
	}

	override def toString() = "Prioritize"
}

/**
 * ReleasePlanView contains a RP view of a given model.
 * @param contain The ModelContainer to be used.
 */
class ReleasePlanView(contain: ModelContainer) extends PanelView(contain) with Progress {
	import java.awt._
	import javax.swing._
	import scala.collection.JavaConverters._
	import java.util.HashMap
	var mainPanel = new JPanel(new BorderLayout()) with WITHGRAYCOLOR
	var mainScroll : JScrollPane = new JScrollPane()
	var d = new Dimension(40, 40)
	mainScroll.setSize(d)
	mainScroll.setPreferredSize(d)
	mainScroll.getVerticalScrollBar().setUnitIncrement(40);
	mainPanel.add(mainScroll, BorderLayout.CENTER)
	var panel = new StructurePanel(mainScroll)
	mainScroll.getViewport().setView(panel)
	var prioFields = new HashMap[Entity, NumberField]
	var stakeholders = new HashMap[String, JMenu]
	var methodMap = new HashMap[String, Entity]
	var stakeMethod = new HashMap[String, HashSet[String]]
	var DEFAULTWIDTH = 200
	var isRuhe = new JCheckBox("Multiply priority by urgency (Method: by Ruhe)")
	var combo: JComboBox[Object] = _
	var addButton = new ActionButton("Add", new ActionListener() {
		/** Add release **/
		override def actionPerformed(a: ActionEvent) {
			if (combo.getSelectedIndex != 0) {
				var selected = combo.getSelectedItem()
				if (selected != null) {
					var entity = selected.asInstanceOf[Entity]
					releases += entity
					container.has(entity, Order, "" + releases.size)
				}
			} else {
				var e = (new AddEntity()).doAction(container, Release.toString)
				if (e != null) {
					container.has(e, Order, "" + releases.size)
					releases += e
				}
			}
			updateView()
		}
	})
	pbar.setMinimum(0)
	pbar.setMaximum(11)
	var thread = new Thread() {
		override def run() {
			while (!isInterrupted()) {
				try {
					pbar.setValue(ReleasePlanGeneration.STATUS)
					container.updateGlassPane
					repaint()
					Thread.sleep(400)
				} catch {
					case e: Exception =>
				}
			}
		}
	}
	var pane : JScrollPane = null
	var scroll : JScrollPane = null
	thread.start()
	var releasePanel: StructurePanel = null
	var releases = new ListBuffer[Entity]()
	var nodes = new PrioMap(container)
	var releaseMap = new HashMap[Int, ReleasePanel]()
	var generate = new ActionButton("Generate new model with release plan", new ActionListener() {
		override def actionPerformed(e: ActionEvent) {
			var t = new Thread() {
				override def run() {
					runReleasePlanning()
				}
			}
			t.start()
		}
	}) with Tooltip { override def getMessage(): String = { return "Generates a release plan solution." } }
	var genPanel = new JPanel(new GridLayout(2,1)) with WITHGRAYCOLOR
	genPanel.add(pbar)
	genPanel.add(generate)
	mainPanel.add(genPanel, BorderLayout.SOUTH)
	setView(mainPanel)

	def this() = this(new ModelContainer(Model()))

	def this(model: Model) = this(new ModelContainer(model))

	def this(model: Model, entity: Entity) = this(new ModelContainer(model, entity))

	/** Prepares and starts a release planning **/
	def runReleasePlanning() {
		var start = Platform.currentTime
		container.gui.wake(this)
		var ruhe = isRuhe.isSelected
		var notRuhe = false
		if (ruhe) {
			for (c <- stakeMethod.values.toArray(new Array[HashSet[String]](0))) {
				if (c.contains("Ruhe")) {
					notRuhe = true
				}
			}
			if (notRuhe) {
				if (JOptionPane.YES_OPTION != JOptionPane.showConfirmDialog(container.gui, "At least one stakeholder hasn't prioritized with Ruhe, so we advice you to not continue. Are you sure that you wish to continue?", "Multiplication of priority?", JOptionPane.YES_NO_OPTION)) {
					container.gui.sleep(this)
					return
				}
			}
		}
		try {
			ReleasePlanGeneration.releasePlanning(container, stakeMethod,  releases.toList, ruhe)
		} catch {
			case e: Exception => JOptionPane.showMessageDialog(container.gui, e.getMessage(), "Inconsistency found", JOptionPane.INFORMATION_MESSAGE);
		}
		container.gui.sleep(this)
		updateView()
		println("TOTAL TIME: " + (Platform.currentTime - start) / 1000 + " seconds")
	}

	/** Visualize release plan **/
	override def updateView() {
		releases.clear
		(container.model / Order / Release).entities.foreach{rel => releases += rel}
		panel.clean()
		prioFields.clear()
		panel.addSeparator()
		panel.add(new TitleLabel("Release Plan"))
		panel.addSeparator()
//		panel.add(new ReqLabel("Specify how many releases the release plan should contain, available resources in each release and the importance of each stakeholder."))
//		panel.add(new ReqLabel("The release plan is then based on the stakeholder's prioritizations, the resource allocation of each requirement as well as their relations."))
		panel.add(new ReqLabel("1. Add resources."))
		panel.add(new ReqLabel("2. Add releases to release plan."))
		panel.add(new ReqLabel("3. Input resource capacity for releases."))
		panel.add(new ReqLabel("4. Input resource cost for requirements."))
		panel.add(new ReqLabel("5. Input stakeholder importance."))
		panel.add(new ReqLabel("6. Select prioritization methods to use for each stakeholder by clicking on the stakeholder ID."))
		panel.add(new ReqLabel("7. Generate plan."))
		panel.addSeparator()
		var addResourceButton = new ActionButton("Add Resource", new ActionListener() {
			def actionPerformed(e: ActionEvent) {
				var name = JOptionPane.showInputDialog(container.gui, "Enter name of Resource.");
				if (name != null) {
					if (!container.addResource(name)) {
						JOptionPane.showMessageDialog(container.gui, "There already is a Resource named: " + name + ".")
					} else {
						updateView()
					}
				}
			}
		})
		panel.addWest(addResourceButton, DEFAULTWIDTH)
		panel.addSeparator()
		addReleases()
//		panel.addSeparator()
		addRequirements()
		panel.addSeparator()
		addStakeholders()
		panel.addWest(isRuhe)
		panel.resized()
		mainScroll.getViewport().revalidate()
		try{
			updateUI()
			revalidateView()
		}catch{
			case e : Exception => reqt.warn(e.getMessage())
		}
		
	}

	/** Add releases **/
	def addReleases() {
		panel.add(new TitleLabel("Releases"))
		panel.addSeparator()
		panel.add(new ReqLabel("Select releases to include in the release plan. "))
		var list = new ListBuffer[Object]()
		releaseMap.clear()
		list += "New release"
		list = (list ++ ((container.model / Release).entities -- releases))
		//list = list ++ (container.model.entitiesOfKind(Release).toSet[Entity] -- releases)
		combo = new JComboBox(list.toArray)
		var addPanel = new JPanel(new BorderLayout()) with WITHGRAYCOLOR
		addPanel.add(combo, BorderLayout.CENTER)
		addPanel.add(addButton, BorderLayout.EAST)
		panel.addWest(addPanel, DEFAULTWIDTH)
		panel.addSeparator()
		
		var checkValid = (container.model / Release).entities.toSeq
		releases = releases -- releases.diff(checkValid)
		pane = new JScrollPane()
		releasePanel = new StructurePanel(pane)
		releases = releases.sortBy(release => ((container.model / release) !! Order))
		if(releases.size>0){
			panel.add("Release ID and resource capacities. To change the order, drag and drop the ID of a release. To exclude press x.")
		}
		for (nbr <- 0 until releases.size) {
			var rp = new ReleasePanel(releases(nbr), nbr)
			releaseMap.put(nbr, rp)
			releasePanel.addWest(rp, rp.getPreferredSize.width)
		}
		releasePanel.resized
		//var paneSize = new Dimension(Math.min(DEFAULTWIDTH * 3+10, (releasePanel.getPreferredSize().getWidth() * 1.1+30).toInt), Math.min(DEFAULTWIDTH, (releasePanel.getPreferredSize().getHeight() * 1.1+20).toInt))
		var paneSize = new Dimension(700, DEFAULTWIDTH min 60*releases.size)
		pane.setSize(paneSize)
		pane.setPreferredSize(paneSize)
		pane.getViewport().setView(releasePanel)
		panel.addWest(pane)
		panel.addSeparator()
		pane.getViewport().revalidate()
	}
	
	def addRequirements(){
		panel.add(new TitleLabel("Requirements"))
		panel.addSeparator()
		scroll = new JScrollPane()
		scroll.getVerticalScrollBar().setUnitIncrement(40);
		var scrollPanel = new StructurePanel(scroll)
		var reqPanel = new JPanel() with WITHGRAYCOLOR
		panel.add("Requirement ID and resource cost.")
		for(e <- (container.model / Requirement).entities.toList.sortWith(new SortFactory(container).compareEntityId)){
			var req = new RequirementPanel(e)
			reqPanel.add(req)
		}
		reqPanel.setLayout(new GridLayout(reqPanel.getComponentCount, 1))
		scrollPanel.addWest(reqPanel, reqPanel.getPreferredSize.width)
		scrollPanel.resized()
		var d = new Dimension(700, 400)
		scroll.setSize(d)
		scroll.setPreferredSize(d)
		
		scroll.getViewport().setView(scrollPanel)
		panel.addWest(scroll)
		scroll.getViewport().revalidate()
	}

	/** Add stakeholders **/
	def addStakeholders() {
		panel.add(new TitleLabel("Stakeholder"))
		panel.add("Specify the relative importance of the stakeholders. If 0 the stakeholder is ignored.")
//		panel.addSeparator()
		var oldStakeMethod = new HashMap[String, HashSet[String]](stakeMethod)
		stakeMethod.clear()
		var stakeholderPrio = new JPanel() with WITHGRAYCOLOR
		// Loop through all stakeholders with methods
		for (s <- (container.model / has).entitiesOfKind(Stakeholder)) {
			// See saved method configuration for currently existing stakeholders
			if (oldStakeMethod.containsKey(s.id)) {
				stakeMethod.put(s.id, oldStakeMethod.get(s.id))
			} else {
				stakeMethod.put(s.id, new HashSet[String])
			}
			var menuBar = new JMenuBar()
			var menu = new JMenu(s.id) with Tooltip { override def getMessage(): String = "Click to select prioritization methods to use." }
			stakeholders.put(s.id, menu)
			// Add a checkbox for each stakeholder's prio method
			((((container.model / s) !! Submodel) / Subdomain("Prio")) !! Submodel).entities.foreach { method =>
				var box = new JCheckBox(method.id)
				box.addItemListener(new MethodListener(s.id, method.id))
				// If method has not been unchecked, select it.
				if (stakeMethod.containsKey(s.id)) {
					if (!stakeMethod.get(s.id).contains(method.id)) {
						box.setSelected(true)
					}
				}
				menu.add(box)
				methodMap.put(method.id, method)
			}
			menuBar.add(menu)
			var stake = new JPanel(new BorderLayout()) with WITHGRAYCOLOR
			stake.add(menuBar, BorderLayout.NORTH)
			var prio = new NumberField(((container.model / s) !! Prio).toString, "Importance of stakeholder " + s.id, SwingConstants.CENTER)
			prioFields.put(s, prio)
			prio.getDocument().addDocumentListener(new DocumentListener() {
				/** User edits prio of stakeholder. **/
				override def changedUpdate(e: DocumentEvent) { container.hasPrio(s, prio.getValue()) }
				override def removeUpdate(e: DocumentEvent) { container.hasPrio(s, prio.getValue()) }
				override def insertUpdate(e: DocumentEvent) { container.hasPrio(s, prio.getValue()) }
			})
			stake.add(prio, BorderLayout.CENTER)
			var d = new Dimension(Math.max(80, stake.getPreferredSize().getWidth.toInt), stake.getPreferredSize().getHeight.toInt)
			menu.setPreferredSize(new Dimension(d.getWidth.toInt, menu.getPreferredSize.getHeight.toInt))
			stake.setSize(d)
			stake.setPreferredSize(d)
			stakeholderPrio.add(stake)
		}
		stakeholderPrio.setLayout(new GridLayout(stakeholderPrio.getComponentCount() / 7 + 1, 7))
		var left = new JPanel(new BorderLayout()) with WITHGRAYCOLOR
		left.add(stakeholderPrio, BorderLayout.WEST)

		panel.add(left)
	}

	def setValue() {
		pbar.setValue(ReleasePlanGeneration.STATUS)
	}
	def repaintProgress() {
		repaint()
	}
	
	class RequirementPanel(var entity: Entity) extends JPanel with WITHGRAYCOLOR {
		var north = new JPanel(new BorderLayout()) with WITHGRAYCOLOR
		var name = new EntityField(entity, container)
		var s = new Dimension(100 max name.getPreferredSize.width,20)
		name.setPreferredSize(s)
		name.setSize(s)
		north.add(name, BorderLayout.CENTER)
		/*var remove = new TextButton("-", new ActionListener() {
			override def actionPerformed(a: ActionEvent) {
				if (JOptionPane.showConfirmDialog(null, "Are you sure that you do want to remove requirement " + entity.id + " from release plan?", "Remove requirement?", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
					releases -= entity
					updateView()
				}
			}
		}) with Tooltip { override def getMessage(): String = { return "Click to remove requirement " + entity.id + " from release plan." } }
		north.add(remove, BorderLayout.WEST)*/
		add(north)
		for (resource <- container.model.entitiesOfKind(Resource)) {
			var cost = ((container.model / resource) !! Submodel) / entity !! Cost
			var coupled = new CostCoupled(entity, resource, cost)
			add(coupled)
		}
		setLayout(new GridLayout(1, getComponentCount))
	}

	/**
	 * ReleasePanel is the visualisation of a release
	 * @param entity is the Release entity
	 * @param order is the Order of the Release
	 */
	class ReleasePanel(var entity: Entity, var order: Int) extends JPanel with WITHGRAYCOLOR {
		this.addMouseListener(new MouseAdapter() {
			override def mouseExited(e: MouseEvent) {
				updateUI()
			}
		})
		var coupledList = new ListBuffer[CapCoupled]()
		if ((container.model / entity !! Order) != order) {
			container.has(entity, Order, order.toString)
		}
		var north = new JPanel(new BorderLayout()) with WITHGRAYCOLOR
		var name = new EditButton(entity, container)
		name.setPreferredSize(new Dimension(100,30));
		new MoveListener(name, this)
		north.add(name, BorderLayout.CENTER)
		var remove = new TextButton("x", new ActionListener() {
			override def actionPerformed(a: ActionEvent) {
				if (JOptionPane.showConfirmDialog(container.gui, "Are you sure that you do want to remove release " + entity.id + " from release plan?", "Remove release?", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
					releases -= entity
					container.removeHas("Order", entity)
					container.removeHas("Submodel", entity)
					updateView()
				}
			}
		}) with Tooltip { override def getMessage(): String = { return "Click to remove release " + entity.id + " from release plan." } }
		north.add(remove, BorderLayout.WEST)
		add(north)
		for (resource <- container.model.entitiesOfKind(Resource)) {
			var cap = ((container.model / resource) !! Submodel) / entity !! Capacity
			var coupled = new CapCoupled(entity, resource, cap)
			coupledList += coupled
			add(coupled)
		}
		setLayout(new GridLayout(1, getComponentCount))
		setBorder(BorderFactory.createLoweredBevelBorder)
		def setReleaseOrder(order: Int) {
			this.order = order
			container.has(entity, Order, "" + order)
		}
	}

	/** Moves the release from old position to new position
	 * @param old Int is the old position.
	 * @param n Int is the new position.
	 * @param rp is the ReleasePanel to be moved. 
	 */
	def moveRelease(old: Int, n: Int, rp: ReleasePanel) {
		if (old > n) {
			for (i <- old to n by -1) {
				val r = releaseMap.get(i)
				r.setReleaseOrder(i + 1)
				releaseMap.put(r.order, r)
			}
		} else {
			for (i <- old + 1 to n) {
				val r = releaseMap.get(i)
				r.setReleaseOrder(i - 1)
				releaseMap.put(i - 1, r);
			}
		}
		rp.setReleaseOrder(n)
		releaseMap.put(n, rp);
	}

	import javax.swing.event._

	/** MoveListener tracking the movement of a release panel **/
	class MoveListener(val tb: TextButton, val rp: ReleasePanel) extends MouseInputAdapter {
		var startPt = new Point()
		tb.addMouseListener(this)
		tb.addMouseMotionListener(this)

		override def mouseReleased(e: MouseEvent) {
			var c: Component = releasePanel.getComponentAt(SwingUtilities.convertPoint(tb, e.getPoint(), releasePanel))
			if (c.isInstanceOf[ReleasePanel]) {
				val old = rp.order
				val n = c.asInstanceOf[ReleasePanel].order
				if (old != n) {
					moveRelease(old, n, rp)
					updateView()
				}
			}
		}

		override def mouseExited(e: MouseEvent) {
			tb.updateUI();
			rp.updateUI();
		}

		override def mousePressed(e: MouseEvent) {
			startPt = e.getPoint();
		}

		override def mouseDragged(e: MouseEvent) {
			var c: Component = releasePanel.getComponentAt(SwingUtilities.convertPoint(tb, e.getPoint(), releasePanel));
			if (c.isInstanceOf[ReleasePanel]) {
				if (e.getY() < startPt.y) {
					c.getGraphics().fillRect(0, 0, c.getWidth(), 3);
				} else if (e.getY() > startPt.y) {
					c.getGraphics().fillRect(0, c.getHeight() - 3, c.getWidth(), 3);
				}
			} else {
				try {
					c.repaint;
				} catch {
					case e: Exception =>
				}
			}
		}
	};

	/** CapCoupled connects a field with its capacity **/
	class CapCoupled(release: Entity, resource: Entity, cap: Int) extends NumberField("" + cap, "Change capacity of " + resource.id + " for " + release.id + ".") {
		setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(new Color(20, 20, 30, 50)), resource.id, SwingConstants.CENTER, TitledBorder.DEFAULT_POSITION, SMALLFONT))
		setBackground(ALMOSTBACKGROUND)
		getDocument().addDocumentListener(new DocumentListener() {
			override def changedUpdate(e: DocumentEvent) { container.addCapacity(release, resource.id, getValue()) }
			override def removeUpdate(e: DocumentEvent) { container.addCapacity(release, resource.id, getValue()) }
			override def insertUpdate(e: DocumentEvent) { container.addCapacity(release, resource.id, getValue()) }
		})

	}
	/** CostCoupled connects a field with its cost **/
	class CostCoupled(requirement: Entity, resource: Entity, cost: Int) extends NumberField("" + cost, "Change cost of " + resource.id + " for " + requirement.id + ".") {
		setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(new Color(20, 20, 30, 50)), resource.id, SwingConstants.CENTER, TitledBorder.DEFAULT_POSITION, SMALLFONT))
		setBackground(ALMOSTBACKGROUND)
		getDocument().addDocumentListener(new DocumentListener() {
			override def changedUpdate(e: DocumentEvent) { container.addCost(requirement, resource.id, getValue()) }
			override def removeUpdate(e: DocumentEvent) { container.addCost(requirement, resource.id, getValue()) }
			override def insertUpdate(e: DocumentEvent) { container.addCost(requirement, resource.id, getValue()) }
		})
	}

	/** Listener of the method which is selected to be used **/
	class MethodListener(var stakeholderId: String, var methodName: String) extends ItemListener {
		/** Method selection state has changed **/
		def itemStateChanged(e: ItemEvent) {
			if (stakeMethod.containsKey(stakeholderId)) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					stakeMethod.put(stakeholderId, stakeMethod.get(stakeholderId) - methodName)
				} else {
					stakeMethod.put(stakeholderId, stakeMethod.get(stakeholderId) + methodName)
				}
				var color = Color.BLACK
				if (stakeMethod.get(stakeholderId).size > 0) {
					color = new Color(150, 70, 0)
				}
				stakeholders.get(stakeholderId).setForeground(color)
			} else {
				warn("MethodListener in Views tried to access stakeholder " + stakeholderId + " which did not exist!")
			}
		}
	}

	override def toString(): String = "Generate Plan"
}

/**
 * ExtractionView contains a view where you can create a new model by extraction from a given model.
 * @param contain The ModelContainer to be used.
 */
class ExtractionView(contain: ModelContainer) extends PanelView(contain) with Progress{

	val FIELDTOOLTIP = "Add code to extract a model. Example: '/ Feature' to extract a model with all features."
	val DEFAULTWIDTH: Int = 500
	val panel = new StructurePanel(this)
	panel.yDiff = 5
	this.setView(panel)
	var paneSize = new Dimension(DEFAULTWIDTH, DEFAULTWIDTH / 2)

	var fieldList: ListBuffer[JTextField] = new ListBuffer[JTextField]
	fieldList += new TooltipField("", FIELDTOOLTIP)

	val modelName: JTextField = new JTextField("Unnamed") with SelectableField
	var extract = new JButton("Extract")
	extract.addActionListener(new ExtractAction)

	/** updateView updates view when its owner is viewed. **/
	override def updateView() {
		panel.clean()
		if(container.gui!=null){
			container.gui.wake(this)
		}
		panel.addSeparator()
		panel.addTitle("Extract")
		panel.addSeparator()
		val pane = new ScalaArea(container){
			override def updateView(){
				if(isSelected()){
					if(container.gui!=null){
						container.gui.wake(this)
					}
					write()
					if(container.gui!=null){
						container.gui.sleep(this)
					}
					revalidateView()
				}
			}
		}
		pane.setPreferredSize(paneSize)
		pane.updateView()
		panel.addWest(pane, DEFAULTWIDTH)
		panel.addDoubleWest(new TitleLabel("Extraction code "), new ReqLabel("  Ex. '/ Release' or '\\ Status(DROPPED)'"))
		for (field <- fieldList) {
			panel.addWest(field, DEFAULTWIDTH)
		}
		val addRow = new JButton("Add row")
		addRow.addActionListener(new ActionListener() {
			/** Adds a new row with text fields to enable further extraction **/
			override def actionPerformed(e: ActionEvent) { fieldList += new TooltipField("", FIELDTOOLTIP); updateView() }
		})
		panel.addWest(addRow, DEFAULTWIDTH)
		val gridPanel = new JPanel(new GridLayout(1, 2)) with WITHGRAYCOLOR
		gridPanel.add(modelName)
		gridPanel.add(extract)
		panel.addWest(gridPanel, DEFAULTWIDTH)
		panel.resized()
		if(container.gui!=null){
			container.gui.sleep(this)
		}
		revalidateView()
	}

	def isSelected() : Boolean = {
		return container.tempTab!=null&&(container.tempTab._1.toString==toString)
	}
	
	override def setValue() {}
	override def repaintProgress() {
		repaint()
	}
	
	/**	Adds the created model to the GUI */
	class ExtractAction extends ActionListener {
		override def actionPerformed(e: ActionEvent) {
			var tmpModel = container.model
			for (field <- fieldList) {
				tmpModel = Model.interpret(tmpModel.toScala + " " + field.getText);
			}
			var cont = SwingUtilities.getRoot(panel).asInstanceOf[GUI].openModel(modelName.getText, tmpModel)
			cont.unsaved();
			updateView()
		}
	}

	override def toString() = "Extract"

}

/** Layout position of nodes **/
class PrioLayout(contain: ModelContainer, normal : ModelContainer, map: NodeMap) extends Layout(contain, map) {
	
	/** Add entity on map **/
	override def add(entity: Entity): Node = {
		var p = new Point((((container.model / entity) !! Prio)) * diff, (nbr) * diff)
		var n = new Node(entity, container.model, p, map.dLocation, map.zoom, map.selected, nbr)
		nbr += 1
		return n
	}

	/** Return sorted requirements  **/
	override def getEntities(): List[Entity] = {
		return (container.model / Requirement).entities.toList.sortWith(compareEntity)
	}
}

/**
 * EditPrioritizationView edits the importance of stakeholders.
 */
class EditPrioritizationView(contain: ModelContainer) extends PanelView(contain) with Progress {
	import javax.swing.JOptionPane
	import scala.collection.mutable.HashSet
	import javax.swing._
	import java.util.HashMap

	/** 3 hashmaps for selecting stakeholder's prio methods */
	var stakeholders = new HashMap[String, JMenu]
	var methodMap = new HashMap[String, Entity]
	var stakeMethod = new HashMap[String, HashSet[String]]

	pbar.setMinimum(0)
	pbar.setMaximum(8)

	var panel = new StructurePanel(this)
	var tmpContainer = new ModelContainer(Model())
	tmpContainer.name = null
	var prioFields = new HashMap[Entity, NumberField]
	var main = new JPanel(new BorderLayout())  with WITHGRAYCOLOR
	setView(main)
	var entityList = new JList[StakeholderContainer]()
	var timeLimit = new NumberField(Config.getSetting(Config.PRIOTIMELIMITKEY), "Maximum amount of seconds the search will continue before the search ends.")
	var solutionLimit = new NumberField(Config.getSetting(Config.PRIOSOLUTIONLIMITKEY), "Maximum amount of solutions the search will continue before the search ends!")
	entityList.setBackground(ALMOSTBACKGROUND)
	entityList.addListSelectionListener(new ListSelectionListener() {
		/** Selects a Solution **/
		override def valueChanged(e: ListSelectionEvent) {
			var e = entityList.getSelectedValue()
			if(e!=null){
				tmpContainer.model = (((container.model / Subdomain(PRIORITIZATIONS.toString))!!Submodel)/e.entity)!!Submodel
				tmpContainer.name = e.entity.id
				updateView()
			}
		}
	})
	var nodes = new PrioMap(container)
	var generate = new ActionButton("Generate priorities", new ActionListener() {
		override def actionPerformed(e: ActionEvent) {
			var t = new Thread() {
				override def run() {
					runPrioritization(false)
				}
			}
			t.start()
		}
	})

	def runPrioritization(isTest : Boolean) {
		container.gui.wake(this)
		try {
			var (model : Model, r : Result[Any]) = PrioritizationGeneration.prioritization(container, stakeMethod, timeLimit.getValue(), solutionLimit.getValue());
			tmpContainer.model = model;
			tmpContainer.name = "Solution"
			if(!isTest){
				JOptionPane.showMessageDialog(container.gui, r.conclusion.toString()+" with "+r.solutionCount+" solutions!", "Solution", JOptionPane.INFORMATION_MESSAGE)
			}
		} catch {
			case e: Exception => e.printStackTrace(); PrioritizationGeneration.setStatus(0); JOptionPane.showMessageDialog(container.gui, e.getMessage(), "Inconsistency found", JOptionPane.WARNING_MESSAGE);
		}
		container.gui.sleep(this)
		nodes.reset();
		updateView();
	}

	override def setValue() {
		pbar.setValue(PrioritizationGeneration.STATUS)
	}
	
	override def repaintProgress() {
		repaint()
	}

	/** Visualize prioritizaztion **/
	override def updateView() {
		panel.clean()
		main.removeAll()
		prioFields.clear()
		main.add(panel, BorderLayout.NORTH)
		entityList.setListData((for(p <- ((container.model / Subdomain(PRIORITIZATIONS.toString))!!Submodel).entities.toList) yield (new StakeholderContainer(p))).sortWith(new SortFactory(container).compareEntityId).toArray)
		panel.addSeparator()
		panel.add(new TitleLabel("Prioritize"))
		panel.addSeparator()
//		panel.add(new ReqLabel("Stakeholders prioritize requirements based on the requirement's value for each stakeholder through various prioritization techniques."))
//		panel.add(new ReqLabel("The stakeholder's prioritization of a requirement is then solved through merging the solutions for the techniques."))
//		panel.addSeparator()
//		panel.add(new ReqLabel("The final prioritization is then the combined prioritization of the stakeholders multiplied by the importance of each stakeholder."))
		panel.add(new ReqLabel("1. Add requirements."))
		panel.add(new ReqLabel("2. Add stakeholders. You need at least one."))
		panel.add(new ReqLabel("3. Prioritize using methods above"))
		panel.add(new ReqLabel("4. Input stakeholder importance."))
		panel.add(new ReqLabel("5. Select methods to use for each stakeholder by clicking on the stakeholder ID."))
		panel.add(new ReqLabel("6. Generate solutions."))
		panel.addSeparator()
		var button = new JButton("Add new Stakeholder")
		button.addActionListener(new ActionListener() {
		/** Add stakeholder **/
		override def actionPerformed(a: ActionEvent) {
				(new AddEntity()).doAction(container, Stakeholder.toString)
				updateView()
			}
		})
		panel.addWest(button, 500)
		panel.addSeparator()
		var oldStakeMethod = new HashMap[String, HashSet[String]](stakeMethod)
		stakeMethod.clear()
		var stakeholderList = container.model.entitiesOfKind(Stakeholder)
		if(stakeholderList.size>0){
			var stakeholderPrio = new JPanel() with WITHGRAYCOLOR
			for (s <- stakeholderList) {
				// See saved method configuration for currently existing stakeholders
				if (oldStakeMethod.containsKey(s.id)) {
					stakeMethod.put(s.id, oldStakeMethod.get(s.id))
				} else {
					stakeMethod.put(s.id, new HashSet[String])
				}
				var menuBar = new JMenuBar()
				var menu = new JMenu(s.id) with Tooltip { override def getMessage(): String = "Click to select methods to use to prioritize." }
				stakeholders.put(s.id, menu)
				// Add a checkbox for each stakeholder's prio method
				((((container.model / s) !! Submodel) / Subdomain("Prio")) !! Submodel).entities.foreach { method =>
					var box = new JCheckBox(method.id)
					box.addItemListener(new MethodListener(s.id, method.id))
					// If method has not been unchecked, select it.
					if (stakeMethod.containsKey(s.id)) {
						if (!stakeMethod.get(s.id).contains(method.id)) {
							box.setSelected(true)
						}
					}
					menu.add(box)
					methodMap.put(method.id, method)
				}
				menuBar.add(menu)
	
				if (((container.model / s) / Prio).entities.isEmpty) {
					container.hasPrio(s, 0)
				}
				var stake = new JPanel(new BorderLayout()) with WITHGRAYCOLOR
				stake.add(menuBar, BorderLayout.NORTH)
				var prio = new NumberField(((container.model / s) !! Prio).toString, "Importance of stakeholder " + s.id, SwingConstants.CENTER)
				prioFields.put(s, prio)
				prio.getDocument().addDocumentListener(new DocumentListener() {
					/** User edits attribute. **/
					override def changedUpdate(e: DocumentEvent) { container.hasPrio(s, prio.getValue()) }
					override def removeUpdate(e: DocumentEvent) { container.hasPrio(s, prio.getValue()) }
					override def insertUpdate(e: DocumentEvent) { container.hasPrio(s, prio.getValue()) }
				})
				stake.add(prio, BorderLayout.CENTER)
				var d = new Dimension(Math.max(80, stake.getPreferredSize().getWidth.toInt), stake.getPreferredSize().getHeight.toInt)
				menu.setPreferredSize(new Dimension(d.getWidth.toInt, menu.getPreferredSize.getHeight.toInt))
				stake.setSize(d)
				stake.setPreferredSize(d)
				stakeholderPrio.add(stake)
			}
			stakeholderPrio.setLayout(new GridLayout(stakeholderPrio.getComponentCount() / 7 + 1, 7))
			panel.add(new TitleLabel("Stakeholder importance & methods"))
//			panel.addSeparator()
			panel.add("Specify each stakeholder's importance in the fields below. If 0 the stakeholder is ignored.")
//			panel.addSeparator()
			var left = new JPanel(new BorderLayout()) with WITHGRAYCOLOR
			left.add(stakeholderPrio, BorderLayout.WEST)
			panel.add(left)
			var options = new JPanel(new GridLayout(2,2)) with WITHGRAYCOLOR
			options.add(new TitleLabel("Max seconds search"))
			options.add(new TitleLabel("Max solutions (<100)"))
			options.add(timeLimit)
			options.add(solutionLimit)
			panel.addWest(options, 500)
			panel.addWest(generate, 500)
			panel.addWest(pbar, 500)
			panel.addSeparator()
			createSolutionView()
		}
		panel.resized()
		revalidateView()
		main.updateUI()
	}

	def createSolutionView() {
		nodes.nodeLayout = new PrioLayout(tmpContainer, container, nodes)
		var holder = new JPanel(new BorderLayout()) with WITHGRAYCOLOR
		if(!(tmpContainer==null||tmpContainer.name==null)){
			panel.add("Feature ID on the Y-axis, prioritization on the X-axis. Higher priority => more important.")
			panel.add("Click on a solution in the list to the left to view it in the map. The selected solution is colored, other possible values are white.")
			panel.addSeparator()
			holder.add(new TitleLabel(tmpContainer.name, SwingConstants.CENTER), BorderLayout.NORTH)
			var listContainer = new JPanel(new BorderLayout()) with WITHGRAYCOLOR
			var dim = new Dimension(170, 20)
			var name = new TitleLabel("Solutions", SwingConstants.CENTER)
			name.setSize(dim)
			name.setPreferredSize(dim)
			listContainer.add(name, BorderLayout.NORTH)
			listContainer.add(new JScrollPane(entityList), BorderLayout.CENTER)
			listContainer.setBorder(BorderFactory.createEmptyBorder(0, 5, 5, 20))
			main.add(listContainer, BorderLayout.WEST)
			holder.add(nodes, BorderLayout.CENTER)
			main.add(holder, BorderLayout.CENTER)
		}
		nodes.updateView()
	}
	import java.awt.event._

	/** Listener of the method which is selected to be used **/
	class MethodListener(var stakeholderId: String, var methodName: String) extends ItemListener {
		/** Method selection state has changed **/
		def itemStateChanged(e: ItemEvent) {
			if (stakeMethod.containsKey(stakeholderId)) {
				if (e.getStateChange() == ItemEvent.SELECTED) {
					stakeMethod.put(stakeholderId, stakeMethod.get(stakeholderId) - methodName)
				} else {
					stakeMethod.put(stakeholderId, stakeMethod.get(stakeholderId) + methodName)
				}
				var color = Color.BLACK
				if (stakeMethod.get(stakeholderId).size > 0) {
					color = new Color(150, 70, 0)
				}
				stakeholders.get(stakeholderId).setForeground(color)
			} else {
				reqt.warn("MethodListener in Views tried to access stakeholder " + stakeholderId + " which did not exist!")
			}
		}
	}

	override def toString(): String = "Generate Priorities"
}

/** Map of Prioritization **/
class PrioMap(contain: ModelContainer, tmpContainer : ModelContainer) extends NodeMap(contain) {
	def this(contain : ModelContainer) = this(contain, null)
	if(tmpContainer==null){
		nodeLayout = new PrioLayout(new ModelContainer(((container.model/Subdomain(PRIORITIZATIONS.toString))!!Submodel)!!Submodel), contain, this)
	}else{
		nodeLayout = new PrioLayout(tmpContainer, contain, this)
	}
	setBorder(BorderFactory.createLineBorder(Color.BLACK))
	var dSize = new Dimension(800, 300)
	var sub : Model = contain.model
	
	defaultZoom = 5
	reset()

	override def updateView() {
		sub = ((container.model / Subdomain(PRIORITIZATIONS.toString))!!Submodel)
		super.updateView()
	}
	
	/**
	 * Add a node to the map.
	 * @param n is the Node to be added
	 */
	override def addNode(n: Node) {
		super.addNode(n)
		var nbr = n.nbr
		var entities = sub.entities
		for(d <- entities){
			var subd = ((sub/d)!!Submodel)
			var se = (subd/n.entity).entities.toList
			if(!se.isEmpty){
				var e = se(0)
				var p = new Point(((subd/e)!!Prio) * nodeLayout.diff, nbr * nodeLayout.diff)
				if(!points.containsKey(p)){
					var shadowNode = new ShadowNode(e, container.model, p, dLocation, zoom, selected, nbr)
					super.addNode(shadowNode)
				}
			}
		}
	}
	
	override def reset() {
		zoom = defaultZoom
		dLocation = new Point(-2*zoom, -6*zoom)
		updateView()
	}
	
	override def paintComponent(g: Graphics) {
		super.paintComponent(g);
		minimap.updateView()
		g.setFont(TITLEFONT)
		g.setColor(Color.BLACK)
		var prio = "Priority value"
		g.drawString(prio, this.getWidth / 2 - SwingUtilities.computeStringWidth(g.getFontMetrics(), prio) / 2, this.getHeight() - 5)
		g.drawString("reqT ID", 4, 2+g.getFontMetrics().getAscent())
		var nbr = 1
		val colorNbr = new Color(0, 0, 0, 140)
		val colorLine = new Color(0, 0, 0, 30)
		var nodeSize = zoom * nodeLayout.diff
		for (e <- nodeLayout.getEntities) {
			g.setColor(colorNbr)
			g.drawString(e.id, 5, nbr * nodeSize - zoom * (nodeLayout.diff + 1) / 2 - dLocation.y + g.getFontMetrics().getAscent() / 2)
			nbr += 1
		}
		g.setFont(DEFAULTFONT)
		for (nbr <- 0 to getHeight() / (nodeSize) + 1) {
			g.setColor(colorLine)
			g.drawLine(0, nbr * nodeSize - dLocation.y % (nodeSize), getWidth(), nbr * nodeSize - dLocation.y % (nodeSize))
		}
		var prioSpan = getWidth() / (nodeSize)
		for (i <- 1 to prioSpan) {
			/** limit nbrOfNumbers shown on x-axis **/
			if (!(prioSpan > 30 && i % (prioSpan / 15) != 0)) {
				var prio = (i + dLocation.x / (nodeSize))
				g.setColor(colorNbr)
				g.drawString("" + prio, prio * (nodeSize) - dLocation.x + zoom * (nodeLayout.diff - 1) / 2, this.getHeight() - g.getFontMetrics().getAscent() - 10)
				g.setColor(colorLine)
				g.drawLine(prio * (nodeSize) - dLocation.x, 0, prio * (nodeSize) - dLocation.x, getHeight())
			}
		}
	}
}
class ShadowNode(e: Entity, model: Model, p: Point, dPoint: Point, zoom: Int, selected: Entity, n : Int) extends Node(e, model, p, dPoint, zoom, selected, n){
	override def createColor() : Color = {
//		return COLORGENERATOR.brighter(entity)
		return YELLOWCOLOR
	}
}

class PathNumberField(var text: String, tooltip : String, e : Entity, var element : String, var path : reqt.Ref[Model], container : ModelContainer, var stake : Entity, var method : String) extends  NumberEntityField(text, tooltip, e){
	getDocument().addDocumentListener(new DocumentListener() {
		override def changedUpdate(e: DocumentEvent) { setSuspected }
		override def removeUpdate(e: DocumentEvent) { setSuspected }
		override def insertUpdate(e: DocumentEvent) { setSuspected }
	})
	addActionListener(new ActionListener() {
		override def actionPerformed(a: ActionEvent) {
			KeyboardFocusManager.getCurrentKeyboardFocusManager().focusNextComponent()
		}
	})
	def setSuspected() {
		var v = getSuspectValue
		if (v >= 0) {
			var m = Model(e has reqt.attributeFromString(element)("" + v))
			container.updatePathModel(path, m)
		}else{
			container.removeSubmodelPrio(Subdomain(method), stake, e)
		}
	}
}

/**  Used to simplify BorderLayout panels **/
class BorderLayoutFix(c : Component) extends JPanel(new BorderLayout()){
	setOpaque(false)
	add(c, BorderLayout.WEST)
}

class RuheView(contain: ModelContainer) extends PanelView(contain) {

	var stakeBox: JComboBox[Object] = null
	var selectedStakeholder: StakeholderContainer = null
	val DEFAULTWIDTH: Int = 500
	val panel = new StructurePanel(this)
	panel.yDiff = 5
	this.setView(panel)
	var rows = new scala.collection.mutable.ListBuffer[Row]()
	var NAME = "Ratio multi-criteria"
	var tempStakeholder: Entity = null
	override def updateView() {
		panel.clean()
		rows.clear()
		panel.addTitle("Priority and urgency inspired by Ruhe")
		panel.add(new ReqLabel("1. Specify release importance."))
		panel.add(new ReqLabel("2. Select stakeholder."))
		panel.add(new ReqLabel("3. Prioritize by specifying priority and urgency."))
		panel.addSeparator
		panel.add(new ReqLabel("Urgency: which release should the requirement belong to."))
		panel.add(new ReqLabel("The sum of urgencies for a requirement should be less than or equal to 9"))
		panel.add(new ReqLabel("Higher priority => more important."))		
		panel.add(new ReqLabel("Higher urgency in a release => more desired in that release."))
		
		var stakeholders = new ArrayList[StakeholderContainer]()
		for (e <- container.model.entitiesOfKind(Stakeholder)) {
			stakeholders.add(new StakeholderContainer(e))
		}
		stakeBox = new JComboBox[Object](stakeholders.toArray().sortBy(f => (f.asInstanceOf[StakeholderContainer]).entity.id))
		stakeBox.setSelectedItem(selectedStakeholder)
		stakeBox.addActionListener(new ActionListener() {
			override def actionPerformed(e: ActionEvent) {
				selectedStakeholder = stakeBox.getSelectedItem().asInstanceOf[StakeholderContainer]
				if (selectedStakeholder != null) {
					tempStakeholder = selectedStakeholder.entity
				}
				updateView()
			}
		})
		if (tempStakeholder == null && stakeholders.size() > 0 && selectedStakeholder != null) {
			selectedStakeholder = stakeBox.getSelectedItem().asInstanceOf[StakeholderContainer]
			if (selectedStakeholder != null) {
				tempStakeholder = selectedStakeholder.entity
			}
			updateView()
			return
		}
		panel.addSeparator()
		panel.addTitle("Release importance")
		panel.add("Specify the importance of the releases.")
		var releases = contain.model.entitiesOfKind(reqt.Release).toSeq
		var releasePanel = new JPanel() with WITHGRAYCOLOR
		for(release <- releases){
			var prio = ""+((container.model/release)!!Prio)
			var priority = new NumberEntityField(prio, "Edit priority of " + release.id, release) {
				setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(new Color(20, 20, 30, 50)), ""+release.id, SwingConstants.CENTER, TitledBorder.DEFAULT_POSITION, SMALLFONT))
				setBackground(new Color(250, 250, 245))
				getDocument().addDocumentListener(new DocumentListener() {
					override def changedUpdate(e: DocumentEvent) {givePrio }
					override def removeUpdate(e: DocumentEvent) { givePrio }
					override def insertUpdate(e: DocumentEvent) { givePrio }
				})
				def givePrio(){
					var v = getSuspectValue
					if(v>=0){
						container.has(release, Prio, ""+v)
					}
				}
			}
			releasePanel.add(priority)
		}
		releasePanel.setLayout(new GridLayout(1, releasePanel.getComponentCount()))
		panel.addWest(releasePanel, releases.size*75)
		panel.addSeparator()
		panel.addDoubleGridWest(new ReqLabel("Stakeholder:"), stakeBox, DEFAULTWIDTH)
		if (tempStakeholder != null) {
			var m = Model(Subdomain(RUHE.toString))
			container.editSubmodelPrio(m,tempStakeholder)
			var prios = (((contain.model / tempStakeholder !! Submodel) / Subdomain("Prio") !! Submodel) / Subdomain(RUHE.toString) !! Submodel).attributeValueMap(Prio)
			var urgencies = (((contain.model / tempStakeholder !! Submodel) / Subdomain("Prio") !! Submodel) / Subdomain(RUHE.toString) !! Submodel).attributeValueMap(reqt.Urgency)
			panel.addSeparator()
			panel.addTitle("Requirements")
			panel.addSeparator()
			for (req <- (contain.model/Requirement).entities.toList.sortWith((new SortFactory(container)).compareEntityId)) {
				var prio = ""
				if (prios.contains(req)) {
					prio = prios(req) + ""
				}
				var r = new Row(tempStakeholder, req, releases, prio)
				panel.addWest(r, 120 + releases.size * 140)
				rows += r
			}
		}
		panel.resized()
		revalidateView()
		updateUI()
	}

	override def toString(): String = {
		return NAME
	}

	class Row(var s : Entity, var e: Entity, var releases: Seq[Entity], prio: String) extends JPanel  with WITHGRAYCOLOR{

		var map = new scala.collection.mutable.HashMap[Entity, NumberField]()
		var minsize = new Dimension(160, 40)

		var priority = new PathNumberField(prio, "Edit priority of " + e.id, e, "Prio", (s!Subdomain("Prio")!Subdomain(RUHE.toString)!Submodel), container, s, RUHE.toString)
		priority.setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(new Color(20, 20, 30, 50)), "Priority", SwingConstants.CENTER, TitledBorder.DEFAULT_POSITION, SMALLFONT))
		priority.setBackground(new Color(250, 250, 245))
		priority.setPreferredSize(new Dimension((priority.getPreferredSize.width+2) max minsize.width, minsize.height))
		
		var urgencies = (((((contain.model / tempStakeholder !! Submodel) / Subdomain("Prio") !! Submodel) / Subdomain(RUHE.toString) !! Submodel) / e) !! Submodel).attributeValueMap(reqt.Urgency)

		add(new BorderLayoutFix(new EntityField(e, container)))
		add(priority)
		for (r <- releases) {
			var urgency = ""
			if (urgencies.contains(r)) {
				urgency = "" + urgencies(r)
			}
			var u = new PathNumberField(urgency, "Edit urgency of " + e.id, r, "Urgency", (s!Subdomain("Prio")!Subdomain(RUHE.toString)!e!Submodel), container, s, RUHE.toString)
			u.setBorder(BorderFactory.createTitledBorder(BorderFactory.createLineBorder(new Color(20, 20, 30, 50)), "Urgency in " + r.id, SwingConstants.CENTER, TitledBorder.DEFAULT_POSITION, SMALLFONT))
			u.setBackground(new Color(250, 250, 245))
			u.setPreferredSize(minsize)
			
			map += (r -> u)
			add(u)
		}
		setLayout(new GridLayout(1, getComponentCount))

		def getPriority(): Int = {
			if (priority.getText().trim.size > 0) {
				return priority.getValue()
			}
			return -1
		}

		def getUrgency(release: Entity): Int = {
			if (map(release).getText().trim.size > 0) {
				return map(release).getValue
			}
			return -1
		}

	}

}

/**
 * DollarView is the view of the DollarMethod
 * @param contain The ModelContainer to be used.
 */
class DollarView(contain: ModelContainer) extends PanelView(contain) {
import java.util.HashMap
	val DEFAULTWIDTH: Int = 500
	val panel = new StructurePanel(this)
	panel.yDiff = 5
	var stakeBox: JComboBox[Object] = null
	var money = new NumberField("", "Enter total amount of dollars which is allowed to be spent!")
	money.getDocument().addDocumentListener(new DollarUpdater(this) {
		override def changedUpdate(e: DocumentEvent) { updateCapacity(money.getValue) }
		override def removeUpdate(e: DocumentEvent) { updateCapacity(money.getValue) }
		override def insertUpdate(e: DocumentEvent) { updateCapacity(money.getValue) }
	})
	var capacity = 100
	this.setView(panel)
	var entities: HashMap[EntityField, NumberField] = new HashMap[EntityField, NumberField]()
	var text = new JLabel("")
	var dollarsLeft = new GiganticLabel(" ")
	var selectedStakeholder: StakeholderContainer = null
	var tempStakeholder: Entity = null
	var constraints: JButton = null

	/**
	 * Get amount of dollars which are left.
	 * @return Int dollars which are left to spend.
	 */
	def getDollarsLeft(): Int = {
		var total = money.getValue()
		for (entity <- entities.keySet()) {
			total -= entities.get(entity).getValue()
		}
		return total
	}

	/** Update the amount dollars which are left. **/
	def updateDollarsLeft() {
		var dollars = getDollarsLeft()
		dollarsLeft.setText("$" + dollars + " left!")
		if (dollars < 0) {
			dollarsLeft.setForeground(Color.RED)
		} else {
			dollarsLeft.setForeground(Color.BLACK)
		}
		try {
			updateUI()
		} catch {
			case e: Exception =>
		}
	}

	def updateCapacity(newCap: Int) {
		if (selectedStakeholder != null) {
			if (newCap != capacity) {
				capacity = newCap
				container.addDollarCap(newCap, selectedStakeholder.entity)
				updateDollarsLeft()
			}
		}
	}

	/** Updates view **/
	def updateView() {
		text.setText("")
		entities.clear()
		var stakeholders = new ArrayList[StakeholderContainer]()
		for (e <- ((container.model / Stakeholder) \ Relation).entities) {
			stakeholders.add(new StakeholderContainer(e))
		}
		//updateDollarsLeft()
		panel.clean()
		panel.addTitle("100$ method")
		panel.add(new ReqLabel("1. Select stakeholder."))
		panel.add(new ReqLabel("2. Specify available dollars."))
		panel.add(new ReqLabel("3. Prioritize by distributing the dollars among the requirements."))
		panel.add(new ReqLabel("More dollars => higher value."))
		panel.addSeparator

		panel.addWest(dollarsLeft, DEFAULTWIDTH)
		stakeBox = new JComboBox[Object](stakeholders.toArray().sortBy(f => (f.asInstanceOf[StakeholderContainer]).entity.id))
		stakeBox.setSelectedItem(selectedStakeholder)
		stakeBox.addActionListener(new ActionListener() {
			override def actionPerformed(e: ActionEvent) {
				selectedStakeholder = stakeBox.getSelectedItem().asInstanceOf[StakeholderContainer]
				if (selectedStakeholder != null) {
					tempStakeholder = selectedStakeholder.entity
				}
				updateView()
			}
		})
		panel.addDoubleGridWest(new ReqLabel("Stakeholder:"), stakeBox, DEFAULTWIDTH)
		var oldPrio: Map[reqt.Entity, Int] = null
		if (selectedStakeholder != null) {
			tempStakeholder = selectedStakeholder.entity
		} else {
			tempStakeholder = null
		}
		if(tempStakeholder != null){
			var m = Model(Subdomain(DOLLAR.toString))
			container.editSubmodelPrio(m,tempStakeholder)
			panel.addDoubleGridWest(new ReqLabel("Upper limit dollars:"), money, DEFAULTWIDTH)
			panel.addSeparator()
			var cap = capacity
			if (tempStakeholder != null) {
				oldPrio = (((contain.model / tempStakeholder !! Submodel) / Subdomain("Prio") !! Submodel) / Subdomain(DOLLAR.toString) !! Submodel).attributeValueMap(Prio)
				cap = (((contain.model / tempStakeholder !! Submodel) / Subdomain("Prio") !! Submodel) / Subdomain(DOLLAR.toString) !! reqt.Capacity)
				if (cap == 0) {
					cap = 100
				}
			} else {
				cap = 100
			}
			if (capacity != cap || money.getText == "") {
				money.setText(cap + "")
			}
			var entList = (container.model / Requirement).entities.toList
			entList = entList.sortBy(_.toString)
			for (e <- entList) {
				var eField = new EntityField(e, container)
				var prio = ""
				if (oldPrio != null && oldPrio.contains(e)) {
					prio += ""+oldPrio(e)
				}
				var text = new PathNumberField(prio, "Dollars spent to get " + e.id+".", e, "Prio", (tempStakeholder!Subdomain("Prio")!Subdomain(DOLLAR.toString)!Submodel), container, tempStakeholder, DOLLAR.toString)
				text.getDocument().addDocumentListener(new DollarUpdater(this))
				entities.put(eField, text)
				panel.addDoubleGridWest(new BorderLayoutFix(eField), text, DEFAULTWIDTH)
			}
			updateDollarsLeft()
			panel.addSeparator()
		}
		panel.resized()
	}

	def getToString(): String = {
		return toString()
	}

	/** Text description in tab **/
	override def toString(): String = {
		return "Ratio priority"
	}
}

/**
 * OrdinalView is the view of the Ordinal Method
 * @param contain The ModelContainer to be used.
 */
class OrdinalView(contain: ModelContainer) extends PanelView(contain) {
	import java.util.HashMap
	val DEFAULTWIDTH: Int = 500
	val panel = new StructurePanel(this)
	panel.yDiff = 5
	var stakeBox: JComboBox[Object] = null
	this.setView(panel)
	var entities: HashMap[EntityField, NumberField] = new HashMap[EntityField, NumberField]()
	var prios: HashMap[Int, Int] = new HashMap[Int, Int]()
	var text = new JLabel("")
	var selectedStakeholder: StakeholderContainer = null
	var constraints: JButton = null
	var max = 0
	var tempStakeholder: Entity = null

	def isOk(prio: Int): Boolean = {
		return prio >= 0 && prio <= max && (!prios.containsKey(prio) || prios.get(prio) <= 1)
	}
	def change(oldPrio: Int, newPrio: Int) {
		if (!prios.containsKey(oldPrio)) {
			prios.put(oldPrio, 1)
		}
		prios.put(oldPrio, prios.get(oldPrio) - 1)
		put(newPrio)
	}

	def put(prio: Int) {
		if (!prios.containsKey(prio)) {
			prios.put(prio, 0)
		}
		prios.put(prio, prios.get(prio) + 1)
	}

	def isOk(): Boolean = {
		for (v <- prios.keySet()) {
			if ((v != 0) && (!isOk(v))) {
				return false
			}
		}
		return true
	}

	/** Updates view **/
	override def updateView() {
		text.setText("")
		prios.clear()
		entities.clear()
		var stakeholders = new ArrayList[StakeholderContainer]()
		for (e <- (container.model / Stakeholder).entities) {
			stakeholders.add(new StakeholderContainer(e))
		}
		max = (container.model / Requirement).entities.size
		//updateDollarsLeft()
		panel.clean()
		panel.addTitle("Ordinal scale")
		panel.add(new ReqLabel("1. Select stakeholder."))
		panel.add(new ReqLabel("2. Prioritize by giving the requirements a unique value between 0 and the number of requirements."))
		panel.add(new ReqLabel("Higher value => more important."))
		panel.addSeparator
		stakeBox = new JComboBox[Object](stakeholders.toArray().sortBy(f => (f.asInstanceOf[StakeholderContainer]).entity.id))
		stakeBox.setSelectedItem(selectedStakeholder)
		stakeBox.addActionListener(new ActionListener() {
			override def actionPerformed(e: ActionEvent) {
				selectedStakeholder = stakeBox.getSelectedItem().asInstanceOf[StakeholderContainer]
				if (selectedStakeholder != null) {
					tempStakeholder = selectedStakeholder.entity
				}
				updateView()
			}
		})
		panel.addDoubleGridWest(new ReqLabel("Stakeholder:"), stakeBox, DEFAULTWIDTH)
		panel.addSeparator()
		var oldPrio: Map[reqt.Entity, Int] = null
		if (tempStakeholder != null) {
			var m = Model(Subdomain(ORDINAL.toString))
			container.editSubmodelPrio(m,tempStakeholder)
			oldPrio = (((contain.model / tempStakeholder !! Submodel) / Subdomain("Prio") !! Submodel) / Subdomain(ORDINAL.toString) !! Submodel).attributeValueMap(Prio)
			var entList = (container.model / Requirement).entities.toList
			entList = entList.sortBy(_.toString)
			for (e <- entList) {
				var eField = new EntityField(e, container)
				var prio = ""
				if (oldPrio != null && oldPrio.contains(e)) {
					prio+="" + oldPrio(e)
				}
				var text = new PathNumberField(prio, "Value of entity.", e, "Prio", (tempStakeholder!Subdomain("Prio")!Subdomain(ORDINAL.toString)!Submodel), container, tempStakeholder, ORDINAL.toString){
							def setColor() {
						if (isOk(getValue())) {
							setBackground(LIGHTGOODCOLOR)
							tooltip = "Value of entity."
						} else {
							setBackground(LIGHTWARNINGCOLOR)
							var value = getValue()
							if (value > max) {
								tooltip = "Warning: All priorities need to be less than the amount of requirements!"
							} else if (value < 0) {
								tooltip = "Warning: A priority needs to be equal to or greater than 0!"
							} else {
								tooltip = "Warning: Priorities need to be unique in an ordinal scale!"
							}
						}
					}
					override def processKeyEvent(e: KeyEvent) {
						val oldVal = getValue()
						super.processKeyEvent(e)
						val newVal = getValue()
						if (oldVal != newVal && (newVal != 0 || getText().trim != "0")) {
							change(oldVal, newVal)
							setColor()
						}
					}
				}

				var value = text.getValue()
				if (value != 0) {
					put(value)
					text.setColor
				}
				entities.put(eField, text)
				panel.addDoubleGridWest(new BorderLayoutFix(eField), text, DEFAULTWIDTH)
			}
		}
		panel.resized()
	}

	def getToString(): String = {
		return toString()
	}

	/** Text description in tab **/
	override def toString(): String = {
		return "Ordinal priority"
	}

	class OrdinalField(text: String, tp: String) extends NumberField(text, tp) {

		def setColor() {
			if (isOk(getValue())) {
				setBackground(LIGHTGOODCOLOR)
				tooltip = "Value of entity."
			} else {
				setBackground(LIGHTWARNINGCOLOR)
				var value = getValue()
				if (value > max) {
					tooltip = "Warning: All priorities need to be less than the amount of requirements!"
				} else if (value < 0) {
					tooltip = "Warning: A priority needs to be equal to or greater than 0!"
				} else {
					tooltip = "Warning: Priorities need to be unique in an ordinal scale!"
				}
			}
		}

		override def processKeyEvent(e: KeyEvent) {
			val oldVal = getValue()
			super.processKeyEvent(e)
			val newVal = getValue()
			if (oldVal != newVal && (newVal != 0 || getText().trim != "0")) {
				change(oldVal, newVal)
				setColor()
			}
		}

	}

}

/** Updates amount of dollars left in DollarView **/
class DollarUpdater(d: DollarView) extends DocumentListener {
	def changedUpdate(e: DocumentEvent) {
		d.updateDollarsLeft()
	}
	def removeUpdate(e: DocumentEvent) {
		d.updateDollarsLeft()
	}
	def insertUpdate(e: DocumentEvent) {
		d.updateDollarsLeft()
	}
}


