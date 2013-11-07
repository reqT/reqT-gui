package gui

import java.awt.BorderLayout
import java.awt.Color
import java.awt.Dimension
import java.awt.Font
import java.awt.Graphics
import java.awt.KeyEventDispatcher
import java.awt.KeyboardFocusManager
import java.awt.Point
import java.awt.event.KeyEvent
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.util.HashMap
import java.awt.event.ComponentAdapter
import scala.collection.JavaConverters._
import javax.swing.BorderFactory
import javax.swing.JPanel
import javax.swing.SwingUtilities
import javax.swing.border.EtchedBorder
import reqt.Entity
import reqt.Feature
import reqt.Model
import reqt.Stakeholder
import java.awt.event.ComponentEvent
import java.awt.event.MouseWheelListener
import java.awt.event.MouseWheelEvent
import java.awt.event.MouseListener
import java.awt.event.MouseMotionListener
import java.util.Random
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import reqt.Submodel
import reqt.Release
import reqt.Requirement
import java.awt.Component
import javax.swing.JLabel
import javax.swing.SwingConstants
import java.awt.GridLayout
import javax.swing.border.TitledBorder
import reqt.Prio
import reqt.Resource
import reqt.Cost
import scala.collection.mutable.ListBuffer
import javax.swing.ImageIcon
import javax.swing._
import java.util.Observer
import java.util.Observable

/**
 * NodeHolder holds the NodeMap.
 * @param contain is the ModelContainer to be used.
 */
class NodeHolder(contain: ModelContainer) extends PanelView(contain) {

	def this() = this(new ModelContainer(Model()))

	def this(model: Model) = this(new ModelContainer(model))

	def this(model: Model, entity: Entity) = this(new ModelContainer(model, entity))

	var panel = new JPanel(new BorderLayout())
	var nodeMap = new SubmodelMap(contain)
	panel.add(nodeMap, BorderLayout.CENTER)
	setView(panel)

	/** updates view. **/
	override def updateView() {
		nodeMap.updateView()
		if (isVisible() && container.entity != null) {
			nodeMap.selected = container.entity
			nodeMap.forceCenter(container.entity)
		}
		panel.updateUI()
		revalidateView()
	}

	/** Gives the name of the View **/
	override def toString(): String = "Graphical"

}

/** Node located on the Node map**/
class Node(var entity: Entity, model: Model, p: Point, dPoint: Point, zoom: Int, selected: Entity, var nbr : Int) extends JPanel with Tooltip {
	setLayout(null)
	var point = p
	var SIZE = 4
	setOpaque(true);
	var borderColor = Color.black;
	var bg = Color.black;
	var prio = "" + ((model / entity) !! Prio)
	setColor(selected)
	var icon : Icon = null
	forceLocation(dPoint, zoom)

	/** Force location of Node **/
	def forceLocation(dPoint: Point, zoom: Int) {
		setLocation(point.x * zoom - dPoint.x, point.y * zoom - dPoint.y)
		var d = new Dimension(SIZE * zoom, SIZE * zoom)
		setSize(d)
		setPreferredSize(d)
	}
	
	def setIcon(icon : Icon){
		this.icon = icon
	}

	/** Paint Node **/
	override def paintComponent(g: Graphics) {
		super.paintComponent(g);
		var f = new Font("Calibri", Font.PLAIN, 1);
		var fm = this.getFontMetrics(f);
		if (!(fm.stringWidth(entity.id) > getWidth() || fm.getHeight() > getHeight())) {
			do {
				f = new Font("Calibri", Font.PLAIN, f.getSize() + 1);
				fm = this.getFontMetrics(f);
			} while (fm.stringWidth(entity.id) + 6 < getWidth() && fm.getHeight() + 6 < getHeight());
			g.setFont(f);
			g.drawString(entity.id, this.getWidth() / 2 - SwingUtilities.computeStringWidth(g.getFontMetrics(), entity.id) / 2, this.getHeight() / 2 + fm.getAscent() / 2);
		}
		if (getWidth > 70) {
			g.setFont(new Font("Calibri", Font.PLAIN, getWidth() / 7))
			g.drawString(prio, this.getWidth() - SwingUtilities.computeStringWidth(g.getFontMetrics(), prio) - 2, g.getFontMetrics().getAscent());
			g.drawString(entity.prefix,  this.getWidth() / 2 - SwingUtilities.computeStringWidth(g.getFontMetrics(), entity.prefix) / 2, g.getFontMetrics().getAscent()/2+g.getFontMetrics().getHeight());
		}
	}

	/** Node tooltip **/
	override def getMessage(): String = {
		return entity.prefix + ": " + entity.id + "\n\nGist: " + ((model / entity) !! reqt.Gist) + "\n\nSpec: " + ((model / entity) !! reqt.Spec)
	}
	
	/** Creates the color to be used as background **/
	def createColor() : Color = {
		return COLORGENERATOR.getColor(entity)
	}
	
	/** Set Color of the Node based on instanceOf Entity **/
	def setColor(selected: Entity) {
		bg = createColor()
		borderColor = bg.darker()
		/*if (entity == selected) {
			bg = bg.brighter().brighter().brighter()
		}*/
		setBackground(bg);
		setBorder(BorderFactory.createLineBorder(borderColor));
	}
	/** Return current color **/
	def getColor(): Color = {
		return bg
	}
}

/**
 * NodeMap is a node map.
 * @param contain is the ModelContainer to be used.
 */
class NodeMap(contain: ModelContainer) extends JPanel with View with WITHYELLOWCOLOR{
	container = contain
	setLayout(null)
	var defaultZoom = 20
	var dLocation = new Point(0, 0)
	var points = new HashMap[Point, Node]()
	var nodes = new HashMap[Entity, Node]()
	var zoom = defaultZoom
	var minimap = new Minimap(this)
	var timeOfEvent: Long = 0
	var mousePress: Point = null
	var nodeLayout = new Layout(container, this)
	var selected: Entity = null

	addMouseWheelListener(new MouseWheelListener() {
		/** Scrolled **/
		override def mouseWheelMoved(w: MouseWheelEvent) {
			var newZoom = Math.max(1, zoom - (1 + Math.min(zoom / 5, 10)) * w.getWheelRotation())
			dLocation.x = newZoom * (dLocation.x + w.getPoint().x) / zoom - (w.getPoint().x)
			dLocation.y = newZoom * (dLocation.y + w.getPoint().y) / zoom - (w.getPoint().y)
			zoom = newZoom
			updateView()
		}
	})

	/** Enables nodeMap to be moved by dragging mouse **/
	addMouseMotionListener(new MouseMotionListener() {
		override def mouseMoved(e: MouseEvent) {}

		override def mouseDragged(e: MouseEvent) {
			if (mousePress != null) {
				dLocation.x = dLocation.x - (e.getPoint().x - mousePress.x)
				dLocation.y = dLocation.y - (e.getPoint().y - mousePress.y)
				mousePress = e.getPoint()
				updateView()
			}
		}
	})

	/** used for nodeMap dragging **/
	addMouseListener(new MouseListener() {
		override def mouseClicked(e: MouseEvent) {}
		override def mouseEntered(e: MouseEvent) {}
		override def mouseExited(e: MouseEvent) {}

		override def mousePressed(e: MouseEvent) {
			mousePress = e.getPoint()
		}

		override def mouseReleased(e: MouseEvent) {
			mousePress = null
		}
	})

	var manager = KeyboardFocusManager.getCurrentKeyboardFocusManager();
	manager.addKeyEventDispatcher(new KeyEventDispatcher() {
		/** If key has been pressed the node map's location should be moved. **/
		override def dispatchKeyEvent(e: KeyEvent): Boolean = {
			if (hasFocus()) {
				var v = zoom;
				if (e.getKeyChar() == 'w') {
					dLocation.y -= v
				} else if (e.getKeyChar() == 'a') {
					dLocation.x -= v
				} else if (e.getKeyChar() == 'd') {
					dLocation.x += v
				} else if (e.getKeyChar() == 's') {
					dLocation.y += v
				} else if (e.getKeyChar() == 'm') {
					minimap.minimize
				}
				updateView()
			}
			return false;
		}
	})

	/** resets zoom and position of NodeMap **/
	def reset() {
		zoom = defaultZoom
		dLocation = new Point(0, 0)
		updateView()
	}

	/** updates the view **/
	override def updateView() {
		removeAll()
		add(minimap)
		nodeLayout.clear()
		points.clear()
		nodes.clear()
		var dpos = new Point(zoom, zoom)
		var diff = 5
		if (container.model != null) {
			for (e <- nodeLayout.getEntities()) {
				addNode(nodeLayout.add(e))
			}
		}
		try{
			updateUI()
		}catch{
			case e : Exception => println(e.getMessage)
		}
		requestFocus()
	}

	/**
	 * Add a node to the map.
	 * @param e is the Entity to be added as a node
	 */
	def addNode(n: Node) {
		n.addMouseListener(new NodeSelectionListener(n))
		n.forceLocation(dLocation, zoom)
		add(n)
		points.put(n.point, n)
		nodes.put(n.entity, n)
	}

	/**
	 * Force NodeMap to center a specific Node
	 * @param e is the Entity which is contained in centered Node.
	 */
	def forceCenter(e: Entity) {
		if (e != null && nodes.containsKey(e)) {
			selected = e
			// Removed as to remember position on map.
			/* var n = nodes.get(e)
			dLocation.x = (n.point.x * zoom + n.SIZE * zoom) - this.getWidth() / 2
			dLocation.y = (n.point.y * zoom + n.SIZE * zoom) - this.getHeight() / 2*/
			updateView()
		}
	}

	/**
	 * paintChildren paints all children of the NodeMap on top of the NodeMap.
	 * @param g is Graphics which is painted upon.
	 */
	override def paintChildren(g: Graphics) {
		super.paintChildren(g);
	}

	/**
	 * getPoint gives the real point that user clicked upon disregarding movement and zoom.
	 * @param p : Point
	 */
	def getPoint(p: Point): Point = {
		return getPoint(p.x, p.y)
	}

	/**
	 * getPoint gives the real point that user clicked upon disregarding movement and zoom.
	 * @param x is clicked location x on screen.
	 * @param y is clicked location y on screen.
	 */
	def getPoint(x: Int, y: Int): Point = {
		return new Point(((x - dLocation.x) / zoom), ((y - dLocation.y) / zoom));
	}

	/**
	 * isAllowed checks if a given point is allowed to be added on the map.
	 * @param p is the point which a user attempts to add to the map.
	 */
	def isAllowed(p: Point): Boolean = {
		return !points.containsKey(p);
	}

	/** Select node **/
	def select(n: Node) {
		var EVENTPOINT = 400
		if (selected == n.entity) {
			if (System.currentTimeMillis() - timeOfEvent > EVENTPOINT) {
				timeOfEvent = System.currentTimeMillis()
				nodes.get(selected).setColor(null)
				selected = null
				return ;
			} else if (System.currentTimeMillis() - timeOfEvent <= EVENTPOINT) {
				container.entity = n.entity
				container.launch(new EntityView(container))
			}
		}
		timeOfEvent = System.currentTimeMillis()
		if (selected != null && nodes.containsKey(selected)) {
			nodes.get(selected).setColor(n.entity)
		}
		selected = n.entity
		n.setColor(selected)
		minimap.updateUI()
	}
	
	/** Method which selects node if NodeSelectionListener is fired by clicking on Node **/
	def selectEvent(node : Node, e : MouseEvent){
		select(node)
	}

	/** Listen on Node if selected **/
	class NodeSelectionListener(var n: Node) extends MouseAdapter {
		/** Select Node **/
		override def mousePressed(e: MouseEvent) {
			selectEvent(n, e)
		}
	}
}
/** Basic Layout which decides placement of Nodes **/
class Layout(var container: ModelContainer, var map: NodeMap) {
	var diff = 5
	var nbr = 0
	var MAX_WIDTH = (Math.sqrt(container.model.entities.size) + 1).asInstanceOf[Int]

	/** Add entity on map **/
	def add(entity: Entity): Node = {
		var p = new Point(1 + (nbr % MAX_WIDTH) * diff, 1 + (nbr / MAX_WIDTH) * diff)
		var n = new Node(entity, container.model, p, map.dLocation, map.zoom, map.selected, nbr)
		nbr += 1
		return n
	}

	/** Sort by Entity id **/
	def compareEntity(e1: Entity, e2: Entity) = { e1.id.compareTo(e2.id) < 0 }

	/** Return sorted entities  **/
	def getEntities(): List[Entity] = {
		return container.model.entities.toList.sortWith(compareEntity)
	}

	/** Reset Layout **/
	def clear() {
		nbr = 0
		MAX_WIDTH = (Math.sqrt(container.model.entities.size) + 1).asInstanceOf[Int]
	}
}

/**
 * Minimap of nodes
 * @param nodeMap is a NodeMap containing Nodes.
 */
class Minimap(var nodeMap: NodeMap) extends JPanel with Tooltip with WITHGRAYCOLOR{
	var minimized = false
	setLayout(null)
	var SIZE = new Dimension(200, 200)
	setSize(SIZE)
	setPreferredSize(SIZE)
	var minimizeButton = new XButton(new ActionListener() {
		override def actionPerformed(e: ActionEvent) {
			minimize()
		}
	}, "_") { setOpaque(true);}
	minimizeButton.setBackground(GRAYCOLOR)
	add(minimizeButton)
	setBorder(BorderFactory.createEtchedBorder(EtchedBorder.RAISED))
	updateView()
	/** Set location of minimap if nodemap change in regard of size **/
	nodeMap.addComponentListener(new ComponentAdapter() {
		override def componentResized(e: ComponentEvent) {
			updateView()
		}
		override def componentShown(e: ComponentEvent) {
			updateView()
		}
	})

	/** Listen if to move location of view **/
	addMouseListener(new MouseAdapter() {
		override def mousePressed(e: MouseEvent) {
			nodeMap.dLocation.x = e.getX() * nodeMap.zoom - nodeMap.getWidth() / 2
			nodeMap.dLocation.y = e.getY() * nodeMap.zoom - nodeMap.getHeight() / 2
			nodeMap.updateView()
		}
	})
	
	addMouseMotionListener(new MouseAdapter() {
		override def mouseDragged(e: MouseEvent) {
			nodeMap.dLocation.x = e.getX() * nodeMap.zoom - nodeMap.getWidth() / 2
			nodeMap.dLocation.y = e.getY() * nodeMap.zoom - nodeMap.getHeight() / 2
			nodeMap.updateView()
		}
	})

	/** Minimize the minimap to the corner **/
	def minimize() {
		if (!minimized) {
			setSize(ButtonSize)
			setPreferredSize(ButtonSize)
		} else {
			setSize(SIZE)
			setPreferredSize(SIZE)
		}
		minimized = !minimized
		updateView()
	}

	/** Update location of the minimap! **/
	def updateView() {
		if (nodeMap != null && nodeMap.minimap != null) {
			setLocation((nodeMap.getWidth() - getWidth()).asInstanceOf[Int], (nodeMap.getHeight() - getHeight()).asInstanceOf[Int])
		}
		minimizeButton.setLocation((getWidth - ButtonSize.width).asInstanceOf[Int], 2)
	}

	/** Tooltip text **/
	override def getMessage(): String = return "Click on minimap to view clicked area."

	/** Paint minimap **/
	override def paintComponent(g: Graphics) {
		g.setColor(Color.BLACK)
		g.fillRect(0, 0, this.getWidth, this.getHeight)
		super.paintComponent(g);
		for (p <- nodeMap.points.keySet.toArray()) {
			var point = p.asInstanceOf[Point]
			var node = nodeMap.points.get(point)
			var color = node.getColor()
			g.setColor(color)
			g.fillRect(point.x, point.y, node.SIZE - 1, node.SIZE - 1)
		}
		g.setColor(Color.RED)
		g.drawRect(nodeMap.dLocation.x / nodeMap.zoom, nodeMap.dLocation.y / nodeMap.zoom, nodeMap.getWidth / nodeMap.zoom, nodeMap.getHeight / nodeMap.zoom)
	}

}

class ScalaArea(contain: ModelContainer) extends PanelView(contain) with Progress with Observer{

	container.addObserver(this)
	
	override def updateView(){
		if(container.tempTab!=null&&(container.tempTab._1.toString==toString||container.tempTab._1.toString==this.getParent().toString)){
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
	
	def getText() : String = {
		return container.model.toScala
	}
	
	def write(){
		var text = new JTextArea(getText()) with SelectableField with Tooltip { override def getMessage(): String = { "Scala code which generates the given model." } }
		text.setEditable(false)
		text.setBackground(ALMOSTBACKGROUND)
		this.setView(text)
	}
	
	override def setValue() {}
	override def repaintProgress() {
		repaint()
	}
	
	override def toString() : String = {return "Textual"}
	/** Updates view in case modelContainer has changed. **/
	override def update(modelContainer: Observable, o: Object) {
		if(isVisible){
			updateView()
		}
	}
}

/** SubmodelMap allows user to explore submodels of a NodeMap **/
class SubmodelMap(contain : ModelContainer) extends NodeMap(contain){
	nodeLayout = new PathLayout(container, this, null)
	var path : ListBuffer[Entity] = new ListBuffer[Entity]()
	var button = new JButton("Parent")
	button.setSize(button.getPreferredSize)
	button.addActionListener(new ActionListener(){
		override def actionPerformed(a : ActionEvent){
			upLayout()
		}
	})
	button.setLocation(0,0)
	
	def upLayout(){
		path.remove(path.size-1)
		if(path.isEmpty){
			nodeLayout = new PathLayout(container, this, null)
		}else{
			setLayout(path)
		}
		reset()
	}
	
	/** updates the view **/
	override def updateView() {
		super.updateView()
		if(!path.isEmpty){
			add(button)
		}
		try{
			updateUI()
		}catch{
			case e : Exception => println(e.getMessage)
		}
		requestFocus()
	}
	
	/*override def selectEvent(node : Node, e : MouseEvent){
		if(e.getButton()!=3){
			select(node)
		}else{		
			var p = path:+node.entity
			setLayout(p)
		}
	}*/
	
	def submodel(node : Node){
		var p = path:+node.entity
		setLayout(p)
	}
	
	def setLayout(p : ListBuffer[Entity]){
		var ref : reqt.Ref[Model] = reqt.Ref[Model](p.toVector, Submodel)
		try{
			if(!container.model(ref).isEmpty){
				path = p
				setLayout(ref)
				updateView()
			}
		}catch{
			case e : Exception =>
		}
	}
	
	def setLayout(path : reqt.Ref[Model]){
		nodeLayout = new PathLayout(container, this, path)
	}
	
}

/** Basic Layout which decides placement of Nodes **/
class PathLayout(c : ModelContainer, m: SubmodelMap, val path : reqt.Ref[Model]) extends Layout(c, m) {
	/** Return sorted entities  **/
	override def getEntities(): List[Entity] = {
		if(path!=null){
			return container.model(path).entities.toList
		}else{
			return super.getEntities()
		}
	}
	/** Add entity on map **/
	override def add(entity: Entity): Node = {
		var p = new Point(1 + (nbr % MAX_WIDTH) * diff, 1 + (nbr / MAX_WIDTH) * diff)
		var n = new Node(entity, container.model, p, map.dLocation, map.zoom, map.selected, nbr){
			var label = new JLabel(UIManager.getIcon("FileChooser.listViewIcon"))
			label.addMouseListener(new MouseAdapter(){
				override def mousePressed(e : MouseEvent){
					sub()
				}
			})
			def sub(){
				m.submodel(this)
			}
			label.setSize(label.getPreferredSize)
			label.setLocation(2,2)
			if(getWidth>70){
				if(path==null){
					if(!(container.model/entity!!Submodel).entities.isEmpty){
						add(label)
					}
				}else{
					if(!(container.model(path)/entity!!Submodel).entities.isEmpty){
						add(label)
					}
				}
			}
		}
		nbr += 1
		return n
	}
}

/**
 * ReleaseBoard is a board representation of a release plan
 * @param contain is the ModelContainer to be used.
 */
class ReleaseBoard(contain: ModelContainer) extends PanelView(contain) {

	def this() = this(new ModelContainer(Model()))

	def this(model: Model) = this(new ModelContainer(model))

	def this(model: Model, entity: Entity) = this(new ModelContainer(model, entity))

	var columns = new ListBuffer[Column]()
	var panel = new JLayeredPane()
	var main = new JPanel(new BorderLayout()) with WITHGRAYCOLOR
	setView(main)
	/** updates view. **/
	override def updateView() {
		columns.clear()
		main.removeAll()
		panel.removeAll()
		var set = (container.model / Requirement).entities
		for (release <- (container.model / Release / reqt.Order).entities.toList.sortWith((new SortFactory(container)).compareEntityOrder)) {
			var tmpSet = ((container.model / release) !! Submodel).entities
			set = set -- tmpSet

			val column = new Column(new ColumnLabel(release, container), tmpSet)
			panel.add(column, JLayeredPane.DEFAULT_LAYER)
			columns += column
		}
		for (entity <- set) {
			var remove = true
			for (resource <- (container.model / Resource).entities) {
				if ((((container.model / resource) !! Submodel) / entity !! Cost) > 0) {
					remove = false
				} 
			}
			if (remove) {
				set = set - entity
			}
		}
		var noRelease = new Column(new ColumnLabel(NORELEASE.toString, container), set)
		panel.add(noRelease, JLayeredPane.DEFAULT_LAYER)
		columns += noRelease
		panel.setLayout(new GridLayout(1, panel.getComponentCount))
		var north = new JPanel()
		north.add(new ReqLabel(" Requirements with a lock icon are locked to a specific release when generating a new release plan."))
		north.add(new ReqLabel(" Click the lock to unlock. Drag the requirement to move it to another release."))
		north.setBackground(ALMOSTBACKGROUND)
		north.setLayout(new BoxLayout(north, BoxLayout.Y_AXIS))
		main.add(north, BorderLayout.NORTH)
		main.add(panel, BorderLayout.WEST)
		panel.revalidate()
		revalidateView()
	}

	/** Gives the name Release board to the View **/
	override def toString(): String = "Release board"
	
	class ColumnLabel(t: String, e: Entity, c: ModelContainer) extends EditLabel(t, e, c){
		def this(entity: Entity, container: ModelContainer) = this(entity.id, entity, container)
		def this(text: String, container: ModelContainer) = this(text, null, container)
		setFont(TITLEFONT)
		setPreferredSize(new Dimension(5, 50))
		var column : Column = null
		
		def setColumn(col : Column){
			column = col
		}
		
		override def paintComponent(g: Graphics) {
			if(column!=null){
				var total : Int = 0
				for(e <- column.orderedList){
					total += ((container.model / e) !! Prio)
				}
				setValue(total)
			}
			super.paintComponent(g)
		}
		
	}
	
	/** A column of entities on the board **/
	class Column(var name: ColumnLabel, entities: Set[Entity]) extends StructurePanel(this) {
		add(name)
		var orderedList = new ListBuffer[Entity]()

		for (e <- entities.toList.sortWith((new SortFactory(container)).compareEntityPrio)) {
			orderedList += e
			var label = new EditLabel((new EntityContainer(e)).toString, e, container) {
				setFont(DEFAULTFONT)
				setPreferredSize(new Dimension(5, 30))
				setBackground(COLORGENERATOR.getColor(e))
				override def paintComponent(g: Graphics) {
					setValue(((container.model / e) !! Prio))
					super.paintComponent(g)
				}
			}
			val listener = new ReleaseDragListener(name.entity, e, label)
			label.addMouseListener(listener)
			label.addMouseMotionListener(listener)
			if (name.getText() != NORELEASE.toString) {
				var lockLabel = new JLabel()
				lockLabel.setPreferredSize(new Dimension(15,30))
				try{
					lockLabel.setIcon(new ImageIcon(this.getClass().getResource("locktrans15.png")))
				}catch{
					case e : Exception => reqt.warn("Image locktrans15.png can't be loaded!")
				}
				lockLabel.addMouseListener(new MouseAdapter() {
					override def mouseClicked(ev: MouseEvent) {
						container.removeSubmodelEntity(name.entity, e)
						updateView()
					}
				})
				addDoubleWest(lockLabel, label, 200)
			} else {
				val noLock = new JPanel(new BorderLayout)
				noLock.setOpaque(false)
				noLock.add(label, BorderLayout.CENTER)
				add(noLock)
			}
		}
		name.setColumn(this)
		resized()

		/** Move requirements between releases **/
		class ReleaseDragListener(oldRel: Entity, e: Entity, label: EditLabel) extends MouseAdapter {
			var dragged = false
			var addToDrag = true
			/** Released entity **/
			override def mouseReleased(ev: MouseEvent) {
				if (dragged) {
					if (panel.getComponentAt(SwingUtilities.convertPoint(label, ev.getPoint, panel)).isInstanceOf[Column]) {
						val editLabel = panel.getComponentAt(SwingUtilities.convertPoint(label, ev.getPoint, panel)).asInstanceOf[Column].name
						if (editLabel.getText != NORELEASE.toString) {
							if (editLabel.entity != oldRel) {
								if (oldRel != null) {
									container.removeSubmodelEntity(oldRel, e)
								}
								container.addSubmodelEntity(editLabel.entity, e)
							}
						} else if (oldRel != null) {
							container.removeSubmodelEntity(oldRel, e)
						}
					}
					updateView()
				}
				dragged = false
				addToDrag = true
			}
			/** Moving the entity **/
			override def mouseDragged(ev: MouseEvent) {
				if (addToDrag) {
					remove(label.getParent())
					panel.add(label.getParent(), JLayeredPane.DRAG_LAYER)
					val panelPoint = SwingUtilities.convertPoint(label, ev.getPoint, panel)
					label.getParent().setLocation(panelPoint.x + 5, panelPoint.y + 5)
					addToDrag = false
					repaint()
				}
				dragged = true
				val panelPoint = SwingUtilities.convertPoint(label, ev.getPoint, panel)
				label.getParent().setLocation(panelPoint.x + 5, panelPoint.y + 5)
				panel.moveToFront(label.getParent())
			}
		}
	}
}
