package gui

import java.awt.Graphics
import javax.swing.SwingUtilities
import java.awt.BorderLayout
import java.awt.Color
import java.awt.Component
import java.awt.Dimension
import java.awt.Font
import java.awt.GridLayout
import java.awt.event.ActionListener
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import java.awt.event.KeyEvent
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import java.util.HashMap
import javax.swing.BorderFactory
import javax.swing.Icon
import javax.swing.JButton
import javax.swing.JComponent
import javax.swing.JLabel
import javax.swing.JPanel
import javax.swing.JScrollPane
import javax.swing.JTextField
import javax.swing.JTextPane
import reqt.Entity
import javax.swing.ImageIcon
import javax.swing.UIManager
import javax.swing.SwingConstants
import javax.swing.ScrollPaneConstants
import scala.collection.mutable.ArrayBuffer
import java.awt.event.ActionEvent
import reqt.Stakeholder
import reqt.Feature
import reqt.Prio
import java.awt.event.FocusAdapter
import java.awt.event.FocusEvent
import javax.swing.JLayeredPane
import java.awt.image.BufferedImage
import javax.swing.text.JTextComponent
import javax.swing.JOptionPane
import java.awt.KeyboardFocusManager
import reqt._
import scala.collection.mutable.Stack
import java.util.Observable
import java.util.Observer
import java.io.File
import java.io.FileInputStream
import java.util.Properties
import java.io.FileOutputStream

/**
 * ModelContainer allows the user to handle the reqT model as a OO mutable Object.
 * Used to have a common model to interact with throughout the GUI.
 */

class ModelContainer(var name: String, var tmpName: String, var path: String, var gui: GUI, var model: Model, var entity: Entity) extends java.util.Observable {

	def this(name: String, tmpName: String, gui: GUI, model: Model, entity: Entity) = this(name, tmpName, "", gui, model, entity)

	def this(m: Model) = this(Config.getSettingAfterReload(Config.DEFAULTMODELKEY), Config.getSettingAfterReload(Config.DEFAULTMODELKEY), null, m, null)

	def this() = this(Model())
	
	def this(name: String, tmpName: String, m: Model) = this(name, tmpName, null, m, null)

	def this(m: Model, e: Entity) = this(Config.getSettingAfterReload(Config.DEFAULTMODELKEY), Config.getSettingAfterReload(Config.DEFAULTMODELKEY), null, m, e)

	val history = new History(this)
	val forwardTabs = new Stack[Tuple2[View, Entity]]()
	val backTabs = new scala.collection.mutable.Stack[Tuple2[View, Entity]]()
	var tempTab: Tuple2[View, Entity] = null
	
	/** Add a tab to be viewable
	 * @param t is a Tuple2[View, Entity] of the View that was viewed and the Entity that was selected.
	 **/
	def addShowTab(t: Tuple2[View, Entity]) {
		forwardTabs.clear
		if (tempTab != null && !(t._1.toString() == tempTab._1.toString() && t._2 == tempTab._2)) {
			backTabs.push(tempTab)
		}
		tempTab = t
	}
	
	/** Goes back to the view that was last viewed **/
	def goBack() {
		if (backTabs.size > 0) {
			if (tempTab != null) {
				forwardTabs.push(tempTab)
				tempTab = null
			}
			val tuple = backTabs.pop()
			tempTab = tuple
			entity = tuple._2
			launch(tuple)
		}
	}
	
	/** Goes forward to the view that back was clicked from. **/
	def goForward() {
		if (forwardTabs.size > 0) {
			if (tempTab != null) {
				backTabs.push(tempTab)
				tempTab = null
			}
			val tuple = forwardTabs.pop()
			tempTab = tuple
			entity = tuple._2
			launch(tuple)
		}
	}
	
	/** copy creates a copy of the ModelContainer**/
	def copy(): ModelContainer = {
		return new ModelContainer(name, tmpName, path, gui, model, entity)
	}
	
	/** Launch history. launch activates the ModelContainer's active gui's launch **/
	def launch(t: Tuple2[View, Entity]) {
		if (gui != null) {
			gui.launch(t._1)
		}
	}

	/** launch activates the ModelContainer's active gui's launch **/
	def launch(v: View) {
		if (v.container == null) {
			v.container = this
		}
		if (gui != null) {
			addShowTab(v, entity)
			gui.launch(v)
		}
	}
	
	/** Opens a model with given model **/
	def openModel(name: String, model: Model): ModelContainer = {
		if (gui != null) {
			return gui.openModel(name, model)
		} else {
			warn("GUI is null in container " + name + ", causing non-thrown Exception in ModelContainer.openModel")
			return null
		}
	}

	/** Activates all observers by declaring unsaved. **/
	def unsaved() {
		setChanged()
		notifyObservers()
		if (gui != null) {
			gui.modelTab.setIconAt(gui.modelTab.indexOfTab(tmpName), UIManager.getIcon("FileView.floppyDriveIcon"))
		} else {
			warn("GUI is null, causing non-thrown Exception in ModelContainer.unsaved().")
		}
	}

	/** hasConstraints **/
	def hasConstraints(entity: Entity, constraints: Vector[Constr[Any]]) {
		model += entity has Constraints(constraints)
		history.doAction(new ModelAction(entity.toScala + " has Constraints ", "+=" + entity.toScala + " has Constraints(" + constraints + ")"))
		unsaved()
	}
	/** hasConstraints **/
	def hasConstraints(entity: Entity, constr: Constr[Any]) {
		hasConstraints(entity, Vector(constr))
	}
	/** addConstraint **/
	def addConstraint(entity: Entity, constr: Constr[Any]) {
		addConstraints(entity, Vector(constr))
	}
	/** addConstraints **/
	def addConstraints(entity: Entity, constraints: Vector[Constr[Any]]) {
		hasConstraints(entity, constraints ++ (model / entity !! Constraints))
	}
	/** hasPrio **/
	def hasPrio(entity: Entity, prio: Int) {
		model += entity has Prio(prio)
		history.doAction(new ModelAction(entity.toScala + " has Prio ", "+=" + entity.toScala + " has Prio(" + prio + ")"))
		unsaved()
	}

	/** Updates gui's glasspane **/
	def updateGlassPane() {
		if (gui != null) {
			gui.updateGlassPane
		}
	}

	/**
	 * Add attribute with has, causes unsaved.
	 * @param reqType is the has attribute used.
	 * @param entity is the entity to be used.
	 * @param value is the value of the attribute.
	 * @example has("Gist", Stakeholder("Joel"), "Is a cool guy")
	 */
	def has(reqType: String, entity: Entity, value: String) {
		var attribute = attributeFromString(reqType)(value)
		model += entity has attribute
		history.doAction(new ModelAction(entity.toScala + " has " + attribute.prefix, "+=" + entity.toScala + " has " + attribute.toScala))
		unsaved()
	}
	/**
	 * Add attribute with has, causes unsaved.
	 * @param entity is the entity to be used.
	 * @param element is the Element of the Attribute which is added.
	 * @param value is the value of the attribute.
	 */
	def has(entity: Entity, element: Element, value: String) {
		has(element.toString, entity, value)
	}

	/**
	 * Add Entity.
	 * @param reqType is the type of the entity.
	 * @param reqName is the name of the entity.
	 */
	def addEntity(reqType: String, reqName: String): Entity = {
		var m = Model.interpret("Model(" + reqType + "(\"" + reqName + "\"))")
		model = model ++ m
		var entity = m.entities.last
		history.doAction(new ModelAction("Added " + entity.toScala, "+=" + entity.toScala))
		unsaved()
		return entity
	}
	/**
	 * Add Entity.
	 * @param entity is the entity to be added.
	 */
	def addEntity(entity: Entity): Entity = {
		model += entity
		history.doAction(new ModelAction("Added " + entity.toScala, "+=" + entity.toScala))
		unsaved()
		return entity
	}

	/**
	 * Remove Entity.
	 * @param entity is the entity to be removed from model.
	 */
	def removeEntity(entity: Entity) {
		model -= entity
		if (this.entity == entity) {
			this.entity = null
		}
		history.doAction(new ModelAction("Removed " + entity.toScala, "-=" + entity.toScala))
		unsaved()
	}

	/**
	 * Update entity type or name
	 * @param oldEntity the entity to be updated
	 * @param newEntity what it should be updated to
	 * @return false if wanted entity id is not unique
	 */
	def updateEntity(oldEntity: Entity, newEntity: Entity): Boolean = {
		if ((oldEntity.id != newEntity.id) && model.ids.contains(newEntity.id)) {
			false
		} else {
			model = model.updateEntities { case e if (e.id == oldEntity.id) => newEntity }
			history.doAction(new ModelAction("Changed entity " + oldEntity.toScala + " to " + newEntity.toScala, "= model.updateEntities{case e if(e.id == \"" + oldEntity.id + "\") => \"" + newEntity + "\"}"))
			unsaved()
			true
		}
	}

	/** Updates selected entity to newEntity **/
	def updateEntity(newEntity: Entity): Boolean = {
		var ok = updateEntity(entity, newEntity)
		if (ok) {
			entity = newEntity
		}
		return ok
	}

	/**
	 * Remove attribute, causes unsaved.
	 * @param reqType is the has attribute used.
	 * @param entity is the entity to be used.
	 * @example removeHas("Gist", Stakeholder("Joel"))
	 */
	def removeHas(reqType: String, entity: Entity) {
		model = (model \ entity) ++ Model.interpret((model / entity).toScala + "-" + reqType)
		history.doAction(new ModelAction(entity.toScala + " removed has " + reqType, "UNKNOWN????")) //TODO: How should command be written?
		unsaved()
	}

	/**
	 * Add relation, causes unsaved.
	 * @param reqType is the relation type used.
	 * @param start is the entity which starts the relation.
	 * @param end is the entity which ends the relation.
	 * @example has("requires", Feature("Car"), Feature("Wheel"))
	 */
	def relation(reqType: String, start: Entity, end: Entity) {
		var m = Model.interpret("Model(" + start.toScala + " " + reqType + " " + end.toScala + ")")
		model = model ++ m
		history.doAction(new ModelAction(start.toScala + " " + reqType + " " + end.toScala, "++" + m.toScala))
		unsaved()
	}

	/**
	 * Remove relation, causes unsaved.
	 * @param reqType is the relation type used.
	 * @param start is the entity which starts the relation.
	 * @param end is the entity which ends the relation.
	 * @example has("requires", Feature("Car"), Feature("Wheel"))
	 */
	def removeRelation(reqType: String, start: Entity, end: Entity) {
		var m = Model.interpret("Model(" + start.toScala + " " + reqType + " " + end.toScala + ")")
		model = model -- m
		history.doAction(new ModelAction(start.toScala + " removed " + reqType + " " + end.toScala, "--" + m.toScala))
		unsaved()
	}

	/** Sets a model to the prioritizations.
	 * @param submodel is the Model of generated prioritizations.
	 */
	def setPrioritization(submodel: Model) {
		model += Subdomain(PRIORITIZATIONS.toString) has Submodel(submodel)
		history.doAction(new ModelAction("Created new prioritization " + new java.util.Date, "+=Subdomain(\"" + PRIORITIZATIONS.toString + "\") has Submodel(" + submodel.toScala + ")"))
		unsaved()
	}
	
	/** Removes the prioritization subdomain.
	 */
	def clearPrioritization() {
		model -= Subdomain(PRIORITIZATIONS.toString)
		history.doAction(new ModelAction("Removed prioritization " + new java.util.Date, "-=Subdomain(\"" + PRIORITIZATIONS.toString + "\")"))
		unsaved()
	}
	
	/** Sets a submodel in a subdomain in the prioritizations.
	 * @param name is the name of the Subdomain to be given the submodel.
	 * @param submodel is the Model of generated prioritizations.
	 */
	def setPrioritization(name : String, submodel: Model) {
		model += Subdomain(PRIORITIZATIONS.toString) has Submodel(Subdomain(name) has Submodel(submodel))
		history.doAction(new ModelAction("Created new prioritization " + name, "TODO")) //TODO: Errenous toScala on model if not used?
		unsaved()
	}

	/**
	 * Add a new kind of resource to the model
	 * @param name of resource
	 * @return false if already exists
	 * @return true if resource has been added
	 */
	def addResource(name: String): Boolean = {
		if (model.entities.contains(Resource(name))) {
			false
		} else {
			model += Resource(name)
			history.doAction(new ModelAction("Resource added: " + name, "+=" + Resource(name).toScala))
			unsaved()
			true
		}
	}

	/**
	 * Rename a resource
	 * @param oldName old name of resource
	 * @param newName new name of resource
	 * @return false if new name already exists
	 * @return true if resource has been edited
	 */
	def renameResource(oldName: String, newName: String): Boolean = {
		if (model.entities.contains(Resource(newName))) {
			false
		} else {
			model = model.replace(Resource(oldName), Resource(newName))
			history.doAction(new ModelAction("Resource name changed from " + oldName + " to " + newName, "= model.replace(Resource(\"" + oldName + "\"),Resource(\"" + newName + "\"))"))
			unsaved()
			true
		}
	}

	/**
	 * Remove a resource from the model
	 * @param name of resource
	 */
	def removeResource(name: String): Boolean = {
		model -= Resource(name)
		history.doAction(new ModelAction("Resource deleted: " + name, "-=" + Resource(name).toScala))
		unsaved()
		true
	}

	/**
	 * Adds a cost for resource of type name to selected entity
	 * @param name of resource
	 * @param cost is the Int value of the cost.
	 */
	def addCost(name: String, cost: Int) {
		addCost(entity, name, cost)
	}

	/**
	 * Adds a cost for resource of type name to selected entity
	 * @param name of resource
	 * @param cost is the Int value of the cost.
	 * @param entity is the Entity to be edited
	 */
	def addCost(entity: Entity, name: String, cost: Int) {
		var resources: Model = (model / Resource(name) !! Submodel)
		resources += entity has Cost(cost)
		model += Resource(name) has Submodel(resources)
		history.doAction(new ModelAction(name + " cost updated for " + entity.toScala, "+=" + Resource(name).toScala + " has Submodel(" + resources.toScala + ")"))
		unsaved()
	}

	/**
	 * Adds a cost for resource of type name to selected entity
	 * @param name of resource
	 * @param cost is the Int value of the cost.
	 * @param entity is the Entity to be edited
	 */
	def addCapacity(entity: Entity, name: String, cost: Int) {
		var resources: Model = (model / Resource(name) !! Submodel)
		resources += entity has Capacity(cost)
		model += Resource(name) has Submodel(resources)
		history.doAction(new ModelAction(name + " capacity updated for " + entity.toScala, "+=" + Resource(name).toScala + " has Submodel(" + resources.toScala + ")"))
		unsaved()
	}

	/**
	 * Adds content to an entities to the Prio submodel Subdomain("Prio") of the selected entity.
	 * @param content model to be added/updated to the Prio submodel
	 */
	def editSubmodelPrio(content: Model) {
		editSubmodelPrio(content, entity)
	}

	/**
	 * Adds content to an entities to the Prio submodel Subdomain("Prio")
	 * @param content model to be added/updated to the Prio submodel
	 * @param entity is the Entity to be modified.
	 */
	def editSubmodelPrio(content: Model, entity: Entity) {
		var oldSub: Model = ((model / entity) !! Submodel) / Subdomain("Prio") !! Submodel
		var newSub = oldSub ++ content
		model += entity has Submodel(Model(Subdomain("Prio") has Submodel(newSub)))
		history.doAction(new ModelAction("Priority updated for " + entity.toScala, "TODO")) //TODO: Errenous toScala on model if not used?
		unsaved()
	}

	/**
	 * Removes an entity prio from a given prio method in Subdomain("Prio")
	 * @param prioMethod
	 * @param stakeholder
	 * @param entity is the Entity to be modified.
	 */
	def removeSubmodelPrio(prioMethod: Entity, stakeholder: Entity, entity: Entity) {
		model -= (stakeholder ! Subdomain("Prio") ! prioMethod ! entity ! Prio)
		history.doAction(new ModelAction("Priority updated for " + entity.toScala, "" /** TODO? **/ ))
		unsaved()
	}
	
	/**
	 * Removes an entity prio from a given prio method in Subdomain("Prio")
	 * @param entity is the Entity to which's submodel is to be removed
	 */
	def removePathAttr(r : reqt.Ref[Any]) {
		model -= r
		history.doAction(new ModelAction("Removed reference " + r.toScala, "-="+r.toScala))
		unsaved()
	}
	/**
	 * Merges the models given by updating the model pointed by the reference
	 */
	def updatePathModel(r : reqt.Ref[Model], m : Model) {
		model = model.updated(r, m)
		history.doAction(new ModelAction("Updated path model " + r.toScala, "TODO!!!"))
		unsaved()
	}
	
	/**
	 * Add capacity for $100 prio for a Stakeholder
	 */
	def addDollarCap(cap: Int, stakeholder: Entity) {
		var oldSub: Model = ((model / entity) !! Submodel) / Subdomain("Prio") !! Submodel
		oldSub += Subdomain("$100") has Capacity(cap)
		model += stakeholder has Submodel(Model(Subdomain("Prio") has Submodel(oldSub)))
		history.doAction(new ModelAction("Available dollars updated for " + stakeholder.toScala, "+=" + stakeholder.toScala + " has " + Submodel(Model(Subdomain("Prio") has Submodel(oldSub))).toScala))
		unsaved()
	}

	/**
	 * Gets the entities located in a submodel of a entity
	 * @param content model is the model the entity is located within.
	 * @param parent is the Entity which has the submodel
	 * @return the entities located in entity's submodel
	 */
	def getSubmodelEntities(content: Model, parent: Entity): Set[Entity] = {
		return ((content / parent) !! Submodel).entities
	}
	/**
	 * Gets the entities located in a submodel of a entity
	 * @param parent is the Entity which has the submodel
	 * @return the entities located in entity's submodel
	 */
	def getSubmodelEntities(parent: Entity): Set[Entity] = {
		return getSubmodelEntities(model, parent)
	}

	/**
	 * Adds child to parent's submodel
	 * @param parent is the Entity which is the parent
	 * @param child is the Entity to be added to parent's submodel
	 */
	def addSubmodelEntity(parent: Entity, child: Entity) {
		var oldSub: Model = (model / parent) !! Submodel
		oldSub += child
		model += parent has Submodel(oldSub)
		history.doAction(new ModelAction(parent.toScala + " given subEntity " + child.toScala, "+=" + parent.toScala + " has " + Submodel(oldSub).toScala))
		unsaved()
	}

	/**
	 * Removes child from parent's submodel
	 * @param parent is the Entity which is the parent
	 * @param child is the Entity to be removed from parent's submodel
	 */
	def removeSubmodelEntity(parent: Entity, child: Entity) {
		var oldSub: Model = (model / parent) !! Submodel
		oldSub = oldSub \ child
		model = (model - (parent ! Submodel)) ++ Model(parent has Submodel(oldSub))
		history.doAction(new ModelAction(parent.toScala + " removed subEntity " + child.toScala, "TODO, if wanted"))
		unsaved()
	}

	/**
	 *  Creates a string for setting an attribute of an entity
	 *  @param attr The type of attribute as a string
	 *  @param v The value to be assigned to the attribute
	 *  @return A string representing the attribute to be added if format is ok, null otherwise
	 */
	def getAttributeFormat(attr: String, v: String): String = {
		def checkType(): Boolean = {
			if (AllAttributes(attr).isInstanceOf[Attribute[_]]) {
				AllAttributes(attr).asInstanceOf[Attribute[_]].value.isInstanceOf[String]
			} else {
				false
			}
		}
		var value = v
		var format = ""
		if (AllAttributes(attr).isInstanceOf[IntValue]) {
			try {
				format = attr + "(" + Integer.parseInt(value) + ")"
			} catch { case e: NumberFormatException => null }
		} else {
			if (AllAttributes(attr).isInstanceOf[StringValue] || checkType()) {
				value = "\"" + value + "\""
			}
			format = attr + "(" + value + ")"
		}
		return format
	}
}

/** Default stucture of a panel **/
class StructurePanel(val scroll: JScrollPane) extends JPanel with WITHGRAYCOLOR{
	var yPos = 0
	var yDiff = 0
	var max = 200
	val componentSize = new HashMap[Component, Dimension]()
	setBorder(BorderFactory.createEmptyBorder(2, 10, 2, 10))
	setLayout(null);
	addComponentListener(new ComponentAdapter() {
		override def componentResized(e: ComponentEvent) {
			resized()
		}
		override def componentShown(e: ComponentEvent) {
			resized()
		}
	})
	/** Adds a space separation in Y-axis **/
	def addSeparator() {
		add(" ")
	}

	/** Adds a title label.
	 * @param title is the title text of the new TitleLabel.
	 */
	def addTitle(title: String) {
		add(new TitleLabel(title))
	}
	/** Adds a label.
	 * @param label is the label to be added. Its given the default font.
	 */
	def addLabel(label: JLabel) {
		label.setFont(DEFAULTFONT)
		add(label)
	}
	/** Adds a label.
	 * @param text is the text of the label.
	 */
	def add(text: String) {
		add(new ReqLabel(text))
	}
	/** Adds a component.
	 * @param comp is the Component to be added.
	 */
	def addComp(comp: Component) {
		add(comp)
	}

	/** Adds components as a west and a center components.
	 * @param title is the Component to be added in the west.
	 * @param value is the Component to be added in the center.
	 */
	def addDoubleWest(title: Component, value: Component) {
		addWest(getDouble(title, value))
	}
	/** Adds components as a west and a center components.
	 * @param title is the Component to be added in the west.
	 * @param value is the Component to be added in the center.
	 * @param preferredWidth is the given width of the components.
	 */
	def addDoubleWest(title: Component, value: Component, preferredWidth: Int) {
		addWest(getDouble(title, value), preferredWidth)
	}
	/** Adds components as a grid.
	 * @param title is the Component to be added first.
	 * @param value is the Component to be added secondly.
	 * @param preferredWidth is the given width of the components.
	 */
	def addDoubleGridWest(title: Component, value: Component, preferredWidth: Int) {
		var panel = new JPanel(new GridLayout(1, 2)) with WITHGRAYCOLOR
		panel.add(title)
		panel.add(value)
		addWest(panel, preferredWidth)
	}

	/** Creates JPanel with a a west and a center component.
	 * @param title is the Component to be added in the west.
	 * @param value is the Component to be added in the center.
	 * @return the JPanel with the components.
	 */
	def getDouble(title: Component, value: Component): Component = {
		var panel = new JPanel(new BorderLayout()) with WITHGRAYCOLOR
		panel.add(title, BorderLayout.WEST)
		panel.add(value, BorderLayout.CENTER)
		return panel
	}
	/** Adds a west and a center labels.
	 * @param title is the Title text to be added in the west.
	 * @param value is the Center text to be added in the center.
	 */
	def addDoubleWest(title: String, value: String) {
		addDoubleWest(new ReqLabel(title), new ReqLabel(value))
	}

	/** Adds ID of Type of Entity
	 * @param entity is the Entity that habve the values to be added. 
	 **/
	def initEntity(entity: Entity) {
		if (entity != null) {
			addDoubleWest("Type: ", entity.prefix)
			addDoubleWest("ID: ", entity.id)
		} else {
			add("Type:")
			add("ID:")
		}
	}
	/** Adds a component to the panel.
	 * @param c is the Component to be added.
	 **/
	def addComponent(c: Component) {
		if (c != null) {
			var size: Dimension = null;
			if (componentSize.containsKey(c)) {
				size = componentSize.get(c)
			} else if (c.getPreferredSize() != null) {
				size = new Dimension(getWidth(), c.getPreferredSize().getHeight().asInstanceOf[Int])
				if (c.getPreferredSize().getWidth() > max) {
					max = c.getPreferredSize().getWidth().asInstanceOf[Int]
				}
			} else {
				size = new Dimension(getWidth(), 5)
			}
			c.setPreferredSize(size)
			c.setSize(size)
			c.setLocation(0, yPos)
			add(c)
			yPos += size.getHeight().asInstanceOf[Int] + yDiff
		}
	}
	/** Adds a component to the panel. Sets the width equal to the preferred width of the component.
	 * @param c is the Component to be added.
	 **/
	def addWest(c: Component) {
		addWest(c, c.getPreferredSize())
	}
	/** Adds a component to the panel.
	 * @param c is the Component to be added.
	 * @param width is the width that the component is forced to have.
	 **/
	def addWest(c: Component, width: Int) {
		addWest(c, new Dimension(width, c.getPreferredSize().getHeight().asInstanceOf[Int]))
		if (width > max) {
			max = width
		}
	}
	/** Adds a component to the panel.
	 * @param c is the Component to be added.
	 * @param size is the Dimension size that the component is forced to have.
	 **/
	def addWest(c: Component, size: Dimension) {
		c.setPreferredSize(size)
		c.setSize(size)
		componentSize.put(c, size)
		addComponent(c)
	}

	/** Resized reorders and resizes the component, should be used when the panel have changed and it should be visualized. **/
	def resized() {
		try{
			var components = this.getComponents()
			clean();
			for (c <- components) {
				addComponent(c)
			}
			updateUI()
			this.setPreferredSize(new Dimension(max, yPos))
			scroll.revalidate()
		}catch{
			case e : Exception => // Block Nimbus exceptions!
		}
	}

	/** Cleans the panel **/
	def clean() {
		yPos = 0
		removeAll()
	}

}

/** Image label **/
class ImageLabel(text: String) extends JLabel(text) {
	import java.io.File
	import javax.swing.ImageIcon
	import java.net.URL
	import java.net.URI

	var SIZE = new Dimension(60,60)
	var originalImage : ImageIcon = null
	var imageLocation : String = null
	setVerticalAlignment(SwingConstants.NORTH)
	setHorizontalAlignment(SwingConstants.CENTER)
	setPreferredSize(SIZE)
	setSize(SIZE)
	addMouseListener(new MouseAdapter(){override def mouseClicked(e : MouseEvent){if(originalImage!=null&&imageLocation!=null){JOptionPane.showMessageDialog(null, "", "Image "+imageLocation, JOptionPane.OK_OPTION, originalImage)}}})
	
	/**
	 * Displays the image of an entity
	 * @param path The path to the image file
	 */
	def setImage(path: String) {
		if (path == null) {
			setIcon(null)
		} else {
			imageLocation = path.trim()
			var url :URL = null
			var file = new File(imageLocation)
			try {
				if (file.exists()) {
					url = file.toURI().toURL()
				} else {
					url = new URI(imageLocation).toURL()
				}
			} catch {
				case e: Exception => println("## Silent exception in ImageLabel: Can't find " + path);
				url = null
			}
			if (url != null) {
				imageLocation = imageLocation.replace("\\", "/")
				
				try {
					var image : ImageIcon = new ImageIcon(url);
					originalImage = image
					var max = Math.max(image.getIconHeight(),image.getIconWidth())
					image = new ImageIcon(image.getImage().getScaledInstance((SIZE.getWidth()*image.getIconWidth()/max).toInt, (SIZE.getHeight()*image.getIconWidth()/max).toInt, java.awt.Image.SCALE_SMOOTH))
					setIcon(image)
					var d = new Dimension(image.getIconWidth(), image.getIconHeight())
					setPreferredSize(d)
					setSize(d)
				} catch {
					case e: Exception => println("## Silent exception in ImageLabel: Can't find " + path);
				}
			} else {
				setIcon(null)
			}
		}
	}
}

object COLORGENERATOR {

	def getColor(entity: Entity): Color = {
		var bg: Color = null
		if (entity.isInstanceOf[Feature]) {
			bg = new Color(255, 140, 110);
		} else if (entity.isInstanceOf[Stakeholder]) {
			bg = new Color(120, 140, 255);
		} else if (entity != null) {
			var rand = new java.util.Random(3 * entity.prefix.toString.hashCode())
			var red = rand.nextInt(255)
			var green = rand.nextInt(255)
			var blue = Math.max(0, 255 - rand.nextInt(red / 2 + green / 2))
			bg = new Color(red, green, blue)
		}
		return bg
	}
	
	def getShadow(entity : Entity) : Color = {
		var c = getColor(entity)
		return new Color(c.getRed, c.getGreen, c.getBlue, 50)
	}
	def brighter(entity : Entity) : Color = {
		var c = getColor(entity)
		var r = c.getRed*1.4 min 255
		var g = c.getGreen*1.4 min 255
		var b = c.getBlue*1.4 min 255
		return new Color(r.toInt, g.toInt, b.toInt)
	}
}

trait WITHGREENCOLOR extends Component{setBackground(GREENCOLOR)}
trait WITHDARKGREENCOLOR extends Component{setBackground(DARKGREENCOLOR)}
trait WITHYELLOWCOLOR extends Component{setBackground(YELLOWCOLOR)}
trait WITHGRAYCOLOR extends Component{setBackground(GRAYCOLOR)}

/** ActionButton **/
class ActionButton(text: String, a: ActionListener) extends JButton(text) { if (a != null) { this.addActionListener(a) } }

/** All attributes in reqT **/
object AllAttributes extends scala.collection.mutable.HashMap[String, reqt.Element] { reqt.attributeKinds.foreach { b => put(b.toString(), b) } }
object AllEntities extends scala.collection.mutable.HashMap[String, reqt.Element] { reqt.entityKinds.foreach { b => put(b.toString(), b) } }

/** Specific Strings **/
class StaticStrings(var value: String) { 
	def updateValue(text : String){value = text}
	override def toString(): String = { return value }
}
object PRIORITIZATIONS extends StaticStrings("Prioritizations")
object RELEASEPLAN extends StaticStrings("Release plan")
object NORELEASE extends StaticStrings("No release")
object DOLLAR extends StaticStrings("$100")
object ORDINAL extends StaticStrings("Ordinal")
object RUHE extends StaticStrings("Ruhe")

object CURRENTDIRECTORY extends StaticStrings("")

/** Default Labels **/
class FontLabel(text: String, value: Int, font: Font) extends JLabel(text, value) {try{this.setFont(font)}catch{case e : Exception => reqt.warn(e.getMessage)}; def this(test: String) = this(test, SwingConstants.LEFT, DEFAULTFONT) }
class ReqLabel(text: String, value: Int) extends FontLabel(text, value, DEFAULTFONT) { def this(test: String) = this(test, SwingConstants.LEFT) }
class TitleLabel(text: String, value: Int) extends FontLabel(text, value, TITLEFONT) { def this(test: String) = this(test, SwingConstants.LEFT) }
class GiganticLabel(text: String, value: Int) extends FontLabel(text, value, GIGANTICFONT) { def this(test: String) = this(test, SwingConstants.LEFT) }

/** Default Fonts **/
object GIGANTICFONT extends Font(new Font("Calibri", Font.BOLD, 36))
object TITLEFONT extends Font(new Font("Calibri", Font.BOLD, 16))
object DEFAULTFONT extends Font(new Font("Calibri", Font.PLAIN, 14))
object MENUFONT extends Font(new Font("Calibri", Font.PLAIN, 14))
object SMALLFONT extends Font(new Font("Calibri", Font.PLAIN, 10))

/** Default Colors **/
object LIGHTWARNINGCOLOR extends Color(251, 235, 206)
object LIGHTGOODCOLOR extends Color(235, 255, 210)
object ALMOSTBACKGROUND extends Color(240, 240, 242)
object GREENCOLOR extends Color(177, 217, 43)
object DARKGREENCOLOR extends Color(129, 179, 13)
object YELLOWCOLOR extends Color(253, 255, 242)
object MENUCOLOR extends Color(21, 41, 4)
object GRAYCOLOR extends Color(245, 242, 238)

/** Entity label **/
class EntityField(var e: Entity, container: ModelContainer) extends ReqLabel(e.prefix + ": " + e.id) with Tooltip { addMouseListener(new MouseAdapter(){override def mouseClicked(event : MouseEvent){container.entity = e;container.launch(new EntityView(container));}}); override def getMessage(): String = { return e.prefix + ": " + e.id + "\n\nGist: " + ((container.model / e) !! reqt.Gist) + "\n\nSpec: " + ((container.model / e) !! reqt.Spec) } }
class NumberEntityField(t : String, tool : String,  var e: Entity) extends NumberField(t, tool){def getSuspectValue() : Int = {var v = getValue();if(v==0&&getText.trim.size==0){return -1}else{return v}}}


/** SortFactory **/
class SortFactory(container: ModelContainer) {

	def compareEntityId(e1: EntityContainer, e2: EntityContainer) = { e1.entity.id.compareTo(e2.entity.id) < 0 }
	def compareEntityId(e1: Entity, e2: Entity) = { e1.id.compareTo(e2.id) < 0 }
	def compareToString(a1: Any, a2: Any) = { a1.toString.compareTo(a2.toString) < 0 }
	def compareEntityPrio(e1: Entity, e2: Entity) = { ((container.model / e1) !! Prio) > ((container.model / e2) !! Prio) }
	def compareEntityOrder(e1: Entity, e2: Entity) = {((container.model / e1)!!reqt.Order)<((container.model / e2)!!reqt.Order)}

}

/** Simple BorderLayoutPanel with sizes **/
class BorderPanel(var west: Component, westSize: Dimension, var center: Component, centerSize: Dimension, var east: Component, eastSize: Dimension, width : Int) extends JPanel with WITHGRAYCOLOR {
	setLayout(new BorderLayout)
	var d = new Dimension(0, 0)
	add(west, BorderLayout.WEST)
	west.setSize(westSize)
	west.setPreferredSize(westSize)
	add(center, BorderLayout.CENTER)
	if(centerSize!=null){
		center.setSize(centerSize)
		center.setPreferredSize(centerSize)
	}
	add(east, BorderLayout.EAST)
	east.setSize(eastSize)
	east.setPreferredSize(eastSize)
	editSize()
	def editSize(){
		d.width = width
		d.height = westSize.height max center.getPreferredSize.height max eastSize.height
		setSize(d)
		setPreferredSize(d)
	}	
}
class SimpleBorderPanel(west: Component, center: Component, east: Component, width : Int) extends BorderPanel(west, west.getPreferredSize(), center, center.getPreferredSize(), east, east.getPreferredSize(), width)

/**
 * StakeholderContainer is the container of Stakeholder used for the ComboBox.
 * @param entity is the stakeholder which is contained.
 */
class StakeholderContainer(entity: Entity) extends EntityContainer(entity){
	/** Text description of container shown in ComboBox. **/
	override def toString(): String = {
		return entity.id
	}
}

class EntityContainer(var entity: Entity) {
	override def toString(): String = {
		if(entity!=null){
			return entity.prefix+" "+entity.id
		}
		return "Empty"
	}

	override def equals(o: Any): Boolean = {
		if (o == null || !o.isInstanceOf[StakeholderContainer]) {
			return false
		}
		return entity.id.equals(o.asInstanceOf[StakeholderContainer].entity.id)
	}

	override def hashCode(): Int = {
		return entity.id.toString.hashCode
	}
}

/** Sortable allows sort by value of a object. **/
class Sortable(var o: Object, var value: Int) extends Comparable[Sortable] {
	override def compareTo(s: Sortable): Int = {
		return value - s.value
	}
	override def toString() : String = {return o.toString}
}

/** SelectedField manages the clipboard interaction. **/
object SelectedField{
	var field : JTextComponent = null
	/** Copy selected text to system clipboard **/
	def copy(){
		if(field!=null){
			field.copy()
		}
	}
	/** Pastes system clipboard text to selected field. **/
	def paste(){
		if(field!=null){
			field.paste()
		}
	}
	/** Cuts the selected text to the clipboard. **/
	def cut(){
		if(field!=null){
			field.cut()
		}
	}
	/** Sets which field that is selected **/
	def select(field : JTextComponent){
		this.field = field
	}
}
/** SelectableField is a trait to describe that the field is interactable with the clipboard. **/
trait SelectableField extends JTextComponent{
	addFocusListener(new FocusAdapter(){
		/** Thrown when field is selected **/
		override def focusGained(e : FocusEvent) {
			select()
		}
	})
	/** Selects field in the SelectField object**/
	def select(){
		SelectedField.select(this)
	}
}

/**
 * TooltipField is a JTextField with a tooltip
 * @param text is the string text shown in the field.
 * @param tooltip is the string tooltip.
 */
class TooltipField(text: String, var tooltip: String) extends JTextField(text) with Tooltip with SelectableField {
	override def getMessage(): String = { return tooltip }
}
/**
 * NumberField only allows numbers to be inserted.
 * @param text is the string text shown in the field.
 * @param tooltip is the string tooltip.
 */
class NumberField(text: String, tooltip: String, align: Int, tabs : Int) extends TooltipField(text, tooltip) {
	def this(text: String, tooltip: String) = this(text, tooltip, SwingConstants.LEFT, 1)
	def this(text: String, tooltip: String, tabs : Int) = this(text, tooltip, SwingConstants.LEFT, tabs)
	setHorizontalAlignment(align)
	addActionListener(new ActionListener() {
		override def actionPerformed(a: ActionEvent) {
			KeyboardFocusManager.getCurrentKeyboardFocusManager().focusNextComponent()
		}
	})
	override def processKeyEvent(e: KeyEvent) {
		var c = e.getKeyChar()
		try {
			if (c > 31 && c < 127) {
				Integer.parseInt(e.getKeyChar() + "")
			}
			super.processKeyEvent(e)
		} catch { case e: Exception => }
	}
	/** get the value of the text as Int **/
	def getValue(): Int = {
		try {
			return Integer.parseInt(getText())
		} catch {
			case e: Exception => return 0;
		}
	}
}
/** NumberField with custom amount of tabbing **/
class NumberTabField(text : String, tooltip : String, tabs : Int)extends NumberField(text, tooltip, SwingConstants.LEFT){
	def this(text : String, tooltip : String) = this(text, tooltip, 1)
	addActionListener(new ActionListener(){
		override def actionPerformed(a : ActionEvent){
			var manager = KeyboardFocusManager.getCurrentKeyboardFocusManager()
			for(i<-0 until tabs){
				manager.focusNextComponent()
			}
		}
	})
}

/** Singleton TooltipPanel in which all tooltips are written. **/
object TooltipPanel extends JScrollPane {

	var main = new JTextPane()
	this.viewport.setView(main)
	var tooltipSize = new Dimension(180, 200)
	setSize(tooltipSize)
	setPreferredSize(tooltipSize)
	main.setEnabled(false)
	main.setDisabledTextColor(new Color(80, 80, 100));
	//main.setBackground(new Color(230, 230, 238))
	main.setBackground(ALMOSTBACKGROUND)

	/**
	 * setTooltip sets the tooltip text of the TooltipPanel.
	 * @param text as String which is to be set as text.
	 */
	def setTooltip(text: String) {
		main.setText(text)
		this.viewport.revalidate()
	}

	/**
	 * setTooltip sets the tooltip text of the TooltipPanel.
	 * @return tooltip as String which is seen in tooltip.
	 */
	def getTooltip(): String = {
		return main.getText()
	}

}

/** Tooltip allows for a Component to have a tooltip. **/
trait Tooltip extends JComponent {

	addMouseListener(new MouseAdapter() {
		/**
		 * Mouse entered the tooltip and thereby triggers getText().
		 * @param MouseEvent which triggered the tooltip.
		 */
		override def mouseEntered(e: MouseEvent) {
			TooltipPanel.setTooltip(getMessage())
		}
	});

	// ID of tooltip.
	var id: String = ""

	/**
	 * Get the ID of the tooltip.
	 * @return id as String of the tooltip.
	 */
	def getId(): String = id

	/**
	 * Returns text to be seen in the tooltip.
	 * @return text as String which is viewed in tooltip.
	 */
	def getMessage(): String = {
		return toString()
	}
}

/** History stores all actions on a model **/
class History(var container: ModelContainer) {

	val arr: ArrayBuffer[ModelAction] = new ArrayBuffer[ModelAction]()
	var nbrOfUndo: Int = 0
	var MAX_SIZE = 100
	var prevAction = ""
	var prevModel = container.model
	var thisAction = ""
	var thisModel = container.model

	/** Undo the most recent active action **/
	def undo() {
		var tempModel = thisModel
		var tempAction = thisAction
		thisModel = prevModel
		prevModel = tempModel
		if (thisAction == "Undo") {
			thisAction = arr.last.actionName
			container.gui.undo.setEnabled(true)
			container.gui.redo.setEnabled(false)
		} else {
			thisAction = "Undo"
			container.gui.undo.setEnabled(false)
			container.gui.redo.setEnabled(true)
		}
		prevAction = tempAction
		container.model = thisModel
		container.unsaved()
	}

	/** Add action to history **/
	def doAction(action: ModelAction): String = {
		if (container.gui != null) {
			/** When action type is changed, the last action is saved and possible to undo**/
			if (action.actionName != thisAction) {
				prevAction = thisAction
				prevModel = thisModel
				thisModel = container.model
				thisAction = action.actionName
				container.gui.undo.setEnabled(true)
				container.gui.redo.setEnabled(false)
			}
			if (nbrOfUndo > 0) {
				arr.remove(arr.size - nbrOfUndo, arr.size - 1)
				nbrOfUndo = 0
			}
			if (!arr.isEmpty && arr.last.actionName == action.actionName) {
				arr.last.actionCommand = action.actionCommand
			} else {
				arr += action
				if (arr.size > MAX_SIZE) {
					arr.remove(0)
				}
			}
			return action.actionCommand
		} else {
			return null
		}
	}

}

/**
 * Represents an action on a model
 *  @param name name of the action
 *  @param command the action
 */
class ModelAction(name: String, command: String) {
	var actionName = name
	var actionCommand = command
}

class TextButton(text: String, listener: ActionListener) extends ActionButton(text, listener) with MouseListener {

	addMouseListener(this)
	setFocusPainted(true)
	//setBorderPainted(false)
	//setContentAreaFilled(false)
	setOpaque(false)
	
	override def mouseClicked(x$1: java.awt.event.MouseEvent): Unit = {}
	override def mouseEntered(x$1: java.awt.event.MouseEvent): Unit = {
		//this.setBorderPainted(true)
		//setContentAreaFilled(true)
	}
	override def mouseExited(x$1: java.awt.event.MouseEvent): Unit = {
		//this.setBorderPainted(false)
		//setContentAreaFilled(false)
	}
	override def mousePressed(x$1: java.awt.event.MouseEvent): Unit = {}
	override def mouseReleased(x$1: java.awt.event.MouseEvent): Unit = {}

}

class EditButton(entity: Entity, container: ModelContainer) extends TextButton(entity.id, new ActionListener() {
	override def actionPerformed(a: ActionEvent) {
		container.entity = entity
		container.launch(new EntityView(container))
	}
}) with Tooltip { override def getMessage(): String = { return "Click to edit " + entity.id + "." } }

class EditLabel(text: String, val entity: Entity, container: ModelContainer) extends JLabel(text, SwingConstants.CENTER) with MouseListener with Tooltip {
	val DEFAULT_VALUE = -9999
	var value = DEFAULT_VALUE
	def this(entity: Entity, container: ModelContainer) = this(entity.id, entity, container)
	def this(text: String, container: ModelContainer) = this(text, null, container)
	setOpaque(true)
	setBackground(Color.WHITE)
	addMouseListener(this)
	setBorder(BorderFactory.createLineBorder(new Color(100, 100, 104)))

	override def mouseClicked(x$1: java.awt.event.MouseEvent): Unit = {
		if (entity != null) {
			container.entity = entity
			container.launch(new EntityView(container))
		}
	}
	override def mouseEntered(x$1: java.awt.event.MouseEvent): Unit = {
		setBackground(getBackground.darker)
	}
	override def mouseExited(x$1: java.awt.event.MouseEvent): Unit = {
		setBackground(getBackground.brighter)
	}
	override def mousePressed(x$1: java.awt.event.MouseEvent): Unit = {}
	override def mouseReleased(x$1: java.awt.event.MouseEvent): Unit = {}

	override def getMessage(): String = {
		if (entity != null) {
			return entity.prefix + ": " + entity.id + "\n\nGist: " + ((container.model / entity) !! reqt.Gist) + "\n\nSpec: " + ((container.model / entity) !! reqt.Spec)
		} else {
			return ""
		}
	}
	
	override def toString() : String = {
		return "EditLabel: "+getText
	}
	
	def setValue(v : Int){
		value = v
	}
	
	def resetValue(){
		value = DEFAULT_VALUE
	}
	
	override def paintComponent(g: Graphics) {
		super.paintComponent(g)
		if(value!=DEFAULT_VALUE){
			var text = "" + value
			g.setFont(SMALLFONT)
			g.drawString(text, this.getWidth() - SwingUtilities.computeStringWidth(g.getFontMetrics(), text) - 2, g.getFontMetrics().getAscent());
		}
	}

}

object ButtonSize extends Dimension(45, 30)
class XButton(listener: ActionListener, text : String) extends TextButton(text, listener) {
	def this(listener : ActionListener) = this(listener, "X")
	setSize(ButtonSize)
	setPreferredSize(ButtonSize)
}


object Config extends HashMap[String, Properties] {

	val DEFAULTPATH = "./GUIConfig"
	val POSITIONPATH = "positions.properties"
	val ICONPATH = "icons.properties"
	val SETTINGSPATH = "settings.properties"
		
	val TESTSETTINGKEY = "TESTSETTINGVALUE"
	val TESTSETTINGVALUE = "Joel is awesome!"

	val DEFAULTMODELKEY = "Default model name"
	val DEFAULTMODELVALUE = "Unnamed"
		
	val PRIOTIMELIMITKEY = "Prioritization search limit in seconds"
	val PRIOTIMELIMITVALUE = "5"
	
	val PRIOSOLUTIONLIMITKEY = "Solution limit of prioritizations"
	val PRIOSOLUTIONLIMITVALUE = "10"
		
	val DEFAULTSPECFIELDKEY = "Spec"
	val DEFAULTCODEFIELDKEY = "Code"
	val DEFAULTCOMMENTFIELDKEY = "Comment"
	val DEFAULTSUBMODELFIELDKEY = "Submodel"
	val DEFAULTWHYFIELDKEY = "Why"
	val DEFAULTPROBLEMFIELDKEY = "Problem"
	val DEFAULTCONSTRAINTSFIELDKEY = "Constraints"
		
	val DEFAULTAREAVALUE = "Area"
	val DEFAULTCONSTRAINTVALUE = "Constraint"
	
	var EXIT_ON_CLOSE = false // Should application exit on close?
	var CAN_SAVE = true // Simulate that ReqTGUI is not allowed to create Config files.
	
	load()
	setDefault(SETTINGSPATH, TESTSETTINGKEY, TESTSETTINGVALUE);
	setDefault(SETTINGSPATH, DEFAULTMODELKEY, DEFAULTMODELVALUE);
	setDefault(SETTINGSPATH, PRIOTIMELIMITKEY, PRIOTIMELIMITVALUE);
	setDefault(SETTINGSPATH, PRIOSOLUTIONLIMITKEY, PRIOSOLUTIONLIMITVALUE);	
	
	setDefault(SETTINGSPATH, DEFAULTSPECFIELDKEY, DEFAULTAREAVALUE);
	setDefault(SETTINGSPATH, DEFAULTCODEFIELDKEY, DEFAULTAREAVALUE);	
	setDefault(SETTINGSPATH, DEFAULTCOMMENTFIELDKEY, DEFAULTAREAVALUE);	
	setDefault(SETTINGSPATH, DEFAULTSUBMODELFIELDKEY, DEFAULTAREAVALUE);	
	setDefault(SETTINGSPATH, DEFAULTWHYFIELDKEY, DEFAULTAREAVALUE);	
	setDefault(SETTINGSPATH, DEFAULTPROBLEMFIELDKEY, DEFAULTAREAVALUE);	
	setDefault(SETTINGSPATH, DEFAULTCONSTRAINTSFIELDKEY, DEFAULTCONSTRAINTVALUE);	
	
	/** set default value of a given key in a specific settings file.
	 * @param path is the file to be given the default value.
	 * @param key is the key to be given a value.
	 * @param value is the value it should be given.
	 */
	def setDefault(path : String, key : String, value : String){
		if(containsKey(path) && !get(path).containsKey(key)){
			get(path).setProperty(key, value)
			if(CAN_SAVE){
				get(SETTINGSPATH).store(new FileOutputStream(DEFAULTPATH+"/"+SETTINGSPATH), "With "+key)
			}
		}
	}
	
	/** getSetting returns the value of the given key in the settingsfile.
	 * @param key is a String of the key to be loaded.
	 */
	def getSetting(key :String) : String = {
		return get(SETTINGSPATH).getProperty(key)
	}

	/** getSettingAfterReload returns the value of the given key in the settings file after reloading values.
	 * @param key is a String of the key to be loaded.
	 */
	def getSettingAfterReload(key :String) : String = {
		load()
		return getSetting(key)
	}
	
	def loadConfig(path: String) {
		try {
			var extendedPath = DEFAULTPATH +"/"+ path
			var config = new File(extendedPath)
			config.createNewFile()
			var properties = new Properties()
			properties.load(new FileInputStream(extendedPath))
			put(path, properties)
		} catch {
			case e : Throwable => println("Error: " + e)
		}
	}
	
	def loadProperty(path : String){
		var properties = new Properties()
		put(path, properties)
	}

	def load() {
		this.clear()
		var f = new File(DEFAULTPATH)
		if (CAN_SAVE&&(f.exists() || f.mkdir())) {
			loadConfig(POSITIONPATH)
			loadConfig(ICONPATH)
			loadConfig(SETTINGSPATH)
		} else {
			println("Warning: ReqTGUI is blocked from making changes to the system.")
			CAN_SAVE = false
			loadProperty(POSITIONPATH)
			loadProperty(ICONPATH)
			loadProperty(SETTINGSPATH)
		}
	}

}
