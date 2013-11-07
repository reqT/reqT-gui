package gui

/** Note: Constraints is ugly and is a simple solution as to show basic theory rather than the perfect solution. **/

import java.text.SimpleDateFormat
import java.util.ArrayList
import java.util.Collections
import java.util.Date
import java.util.HashMap
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.Seq
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import reqt.Capacity
import reqt.Constr
import reqt.Constraints
import reqt.Cost
import reqt.DROPPED
import reqt.Entity
import reqt.Interval
import reqt.Model
import reqt.Order
import reqt.Prio
import reqt.Release
import reqt.Requirement
import reqt.Resource
import reqt.Stakeholder
import reqt.Status
import reqt.Subdomain
import reqt.Submodel
import reqt.Var
import reqt.refToVar
import reqt.seqRefToVar
import reqt.AttributeKind
import reqt.XplusYeqZ

/** PrioritizationGeneration creates a prioritization **/
object PrioritizationGeneration{
	
	@volatile var STATUS = 0
	var prioInterVal = new Interval(0, 20000)
	
	/** Update prioritization generation value **/
	def setStatus(value : Int){
		if(STATUS!=0||value==1){
			STATUS = value
		}
	}
	
	/** Temporary variable **/
	class TmpVar(key : String, parent : Entity, child : Entity) extends Var(parent.id+" "+key+" "+child.id)
	
	/** getPrioritization creates the prioritization constraints for a given ModelContainer
	 * @param container is the ModelContainer which will be modified and used as basis for the constraints.
	 * @param vars is the References which handles the IntVars.
	 * @return List[ReferenceContainer[Int]] of all important IntVars.
	 */
	def getPrioritization(container : ModelContainer, notUsedMethods : HashMap[String, HashSet[String]]) {
		for (r <- (container.model/Prio).entitiesOfKind(Requirement)) {
			container.removePathAttr((r!Prio).asInstanceOf[reqt.Ref[Any]])
		}
		
		setStatus(2)
		var isRuhe = CreateConstraints.generatePrioritizationConstraints(container, notUsedMethods)
		setStatus(3)
		setStatus(4)
		var constrBuffer = new ListBuffer[Constr[Any]]()
		for(r <- (container.model / Requirement).entities){
			var buffer = new ArrayBuffer[Var[String]]()
			container.addConstraint(r, (r!Prio)::Interval(reqt.jacop.Settings.defaultInterval.min, reqt.jacop.Settings.defaultInterval.max))
			for(stakeholder <- (container.model / Stakeholder).entities){
				if(container.model / stakeholder !! Prio != 0){
					var entities = (((container.model / stakeholder) !! Submodel) / r).entities
					if(entities.size<1){
						container.addSubmodelEntity(stakeholder, r)
						entities = (((container.model / stakeholder) !! Submodel) / r).entities
					}
					var shEnt = entities.head
					var imp = Var(stakeholder.id+" impOf "+shEnt.id)
					var prio = (stakeholder!shEnt!Prio)
					constrBuffer+=reqt.XmulYeqZ(prio, (stakeholder!Prio), imp)
					if((((container.model / stakeholder) !! Submodel) / r) !! Status != DROPPED){
						var max = reqt.jacop.Settings.defaultInterval.max
						if(!isRuhe){
							max = (container.model / Requirement).entities.size
						}
						if(reqt.jacop.Settings.defaultInterval.min!=0){
							constrBuffer+=prio::Interval(reqt.jacop.Settings.defaultInterval.min,max)
						}else{
							constrBuffer+=prio::Interval(1,max)
						}
					}else{
						// Dropped should have priority = 0
						constrBuffer+=prio::Interval(0,0)
					}
					buffer+=imp
				}
			}
			reqt.warn("Finished prioritization constraints of "+r.id+".")
			constrBuffer+=reqt.SumEq(buffer.toSeq, (r!Prio))
		}
		container.addConstraints(Subdomain(PRIORITIZATIONS.toString), constrBuffer.toVector)
		setStatus(5)
	}
	
	/** prioritization is the main prioritization function. Given a container it creates a complete prioritization.
	 *  The created prioritization is given to the Subdomain("Prioritizations").
	 * @param container is the ModelContainer which will be modified and used as basis for the constraints.
	 * @return Model of the solutions.
	 */
	def prioritization(container : ModelContainer, notUsedMethods : HashMap[String, HashSet[String]], timeLimit : Int, solutionLimit : Int) : (Model, reqt.Result[Any]) = {
		setStatus(1)
		reqt.jacop.Settings.defaultInterval = prioInterVal
		var time = (new SimpleDateFormat("yyyyMMddHHmmss")).format(new Date());
		var tmpContainer = new ModelContainer(container.model)
		container.removePathAttr((Subdomain(PRIORITIZATIONS.toString)!Submodel.asInstanceOf[reqt.AttributeKind[Any]]))
		container.removePathAttr((Subdomain(PRIORITIZATIONS.toString)!reqt.Constraints.asInstanceOf[AttributeKind[Any]]))
		tmpContainer.hasConstraints(Subdomain(PRIORITIZATIONS.toString), Vector[Constr[Any]]())
		tmpContainer.clearPrioritization()
		PrioritizationGeneration.getPrioritization(tmpContainer, notUsedMethods)
		setStatus(6)
		var (model : Model, r : reqt.Result[Any]) = tmpContainer.model.impose(tmpContainer.model.constraints).solve(reqt.FindAll, Option(timeLimit), Option(solutionLimit))
		setStatus(7)
		r.solutionsOption.foreach(solutions => 
			for(v <- 0 until r.solutionCount.toInt){
				var map = solutions.solutionMap(v)
				var newModel = Model()
				val modelVariables = map collect { case (Var(r @ reqt.Ref(_,_)), i) => (r, i)  } //check r.attrKind is IntValue??
				modelVariables foreach { case (r, i) => newModel = newModel.updated(r, i)  } 
				//container.setPrioritization("#"+v+" at "+time,newModel) 
				container.setPrioritization("Solution #"+v,newModel)
				reqt.warn("Prioritization solution: "+v+" of "+r.solutionCount.toInt)
			}
		)
		setStatus(8)
		//container.setPrioritization(model) // TODO: Fix multiple solutions
		setStatus(0)
		return (model : Model, r : reqt.Result[Any])
	}
	
}

/** Object which generates release plans **/
object ReleasePlanGeneration{
	
	@volatile var STATUS = 0
	var releaseInterval = new Interval(0, 20000)
	
	
	def setStatus(value : Int){
		if(STATUS!=0||value==1){
			STATUS = value
		}
	}
	
	/**getReleasePlanning creates release plan specific constraints. 
	 * @param container is the ModelContainer which will be modified and used as basis for the constraints.
	 * @param releases contains the releases that should be included in the release plan
	 * @param isRuhe determines if the plan should be generated with Ruhe's urgency
	 * @return Model with the release plan
	 */
	def getReleasePlanning(container : ModelContainer, releases : ListBuffer[Entity], isRuhe : Boolean) : Model = {
		
		var sumPrio = Var("Sum of Prioritization");
		
		var lackRelease = Release(NORELEASE.toString) // Order size-1
		container.addEntity(lackRelease)
		releases += lackRelease
		releases.sortWith((new gui.SortFactory(container)).compareEntityOrder) // Sort releases by their starting nbr. Else Scheduling won't work!
		var size = releases.size; // Nbr of releases.
		container.has(lackRelease, reqt.Order, ""+(size-1))		
		for(re <- releases){
			//container.addConstraint(re, re.Order #< size) // Release can't be larger than amount of releases.
			var order = (container.model / re) !! reqt.Order
			container.addConstraint(re, (re!Order)::Interval(order,order)) // Keep the Order
			for(req <- ((container.model / re)!!Submodel).entities){
				container.addConstraint(re, (req!Order)#==(re!Order)) // Keep the Order
			}
			for(dest <- ((container.model / re) / reqt.releases).relationDestinations){
				container.addConstraint(dest, (dest!Order) #== (re!Order))	// Translate requires to constraints.
			}
		}
		setStatus(4)
		var seq = for(re <- releases) yield (re!Order) // Release orders
//		container.addConstraint(Subdomain(RELEASEPLAN.toString), reqt.AllDifferent(seq))// All releases should be sequential.*/
		
		var resources = (container.model / Resource).entities
		var stakeholders = (container.model / Stakeholder).entities
		var requirements = ((container.model / Requirement).entities).toList
		
		// TODO: Inconsistent?
		for(r <- requirements){
			container.model -= (r!Order)
			for(dest <- ((container.model / r) / reqt.requires).relationDestinations){
				container.addConstraint(r, (r!Order) #>= (dest!Order))	// Translate requires to constraints.
			}
			for(dest <- ((container.model / r) / reqt.precedes).relationDestinations){
				container.addConstraint(r, (r!Order) #< (dest!Order))	// Translate precedes to constraints.
			}
			container.addConstraint(r, (r!Order)::Interval(0, size-1))
			for(resource <- resources){
				var cost  : Int = 0
				if(((container.model/resource)!!Submodel).entities.contains(r)){
						cost = (((container.model/resource)!!Submodel)/r)!!Cost
				}else{
					container.addCost(r, resource.id, 0)
				}
				container.addConstraint(r,(resource!r!Cost)::Interval(cost, cost))
			}
		}
		
		setStatus(5)

		var ZERO = Var("0")
		var ONE = Var("1")
		container.addConstraint(Subdomain(RELEASEPLAN.toString), ZERO::Interval(0,0))
		container.addConstraint(Subdomain(RELEASEPLAN.toString), ONE::Interval(1,1))
		
		if(!ConstraintValidation.isConsistent(container)){
			throw new Exception("Inconsistency during early setup of release planning. Inconsistent constraint is: "+ ConstraintValidation.getInconsistent().toString)
		}
		
		setStatus(6)
		
		// Requirements can only be in a release there is enough resources.
		for(resource <- resources){
			/** WARNING: reqT maximizes at about 1000, after defaultInterval.max inconsistency is found. **/
			container.addCapacity(lackRelease, resource.id, 60000)//reqt.jacop.Settings.defaultInterval.max-1) // Lack release can contain all entities!
			for(i <- 0 until releases.size){
				var capacity  : Int = 0
				if(((container.model/resource)!!Submodel).entities.contains(releases(i))){
					capacity = (((container.model/resource)!!Submodel)/releases(i))!!Capacity
				}else{
					container.addCapacity(releases(i), resource.id, 0)
				}
				var capacityRef = (resource!releases(i)!Capacity)
				var tmpRef = Var("Tmp capacity of release "+releases(i).id + " res " + resource.id)
				container.addConstraint(releases(i), (capacityRef)::Interval(capacity,capacity))
				container.addConstraint(releases(i), (tmpRef)::Interval(0,capacity))
				var rects = new ListBuffer[Var[Any]]()
				for(j <- 0 until requirements.size){
					var costRef = (resource!requirements(j)!Cost)
					var reqCostMin = 0
					var reqCostMax = 0
					if(((container.model/resource)!!Submodel).entities.contains(releases(i))){
						reqCostMax = (((container.model/resource)!!Submodel)/requirements(j))!!Cost
						reqCostMin = reqCostMax
					}
					container.addConstraint(requirements(j), (costRef)::Interval(reqCostMin,reqCostMax))
					var capPos = Var("SchedRes by "+requirements(j).id+" of "+resource.id+" in "+releases(i).id)
					container.addConstraint(requirements(j), (capPos)::Interval(0,reqCostMax))

					
					//container.addConstraint(requirements(j), capPos::Interval(0, capacity))
					container.addConstraint(resource, reqt.IfThen((requirements(j)!Order) #== i, reqt.XeqY(capPos, costRef)))
					container.addConstraint(resource, reqt.IfThen((requirements(j)!Order) #!= i,  reqt.XeqC(capPos, 0)))
					rects+=capPos
				}
				container.addConstraint(releases(i), reqt.SumEq(rects.toSeq, tmpRef))
			}
			//container.addConstraint(resource, reqt.Diff2(rects.toVector))
		}
		
		setStatus(7)
		
		if(!ConstraintValidation.isConsistent(container)){
			throw new Exception("Inconsistency after main release planning constraints. Inconsistent constraint is: "+ ConstraintValidation.getInconsistent().toString)
		}
		var minimize = Var("Count to minimize or maximize")
		
		container.addConstraint(Subdomain(RELEASEPLAN.toString), minimize::Interval(-20000000, 200000000))
		var buffer = new ListBuffer[Var[Any]]()
		var vars = HashSet[Var[Any]]()
		((for(c<-container.model.constraints.value) yield(c.variables)).flatten).foreach(v => vars.add(v))
		if(!isRuhe){
			for(re <- requirements){
				var tmp = Var("Prio of "+re.id+" in "+lackRelease.id)
				container.addConstraint(lackRelease, reqt.IfThen((re!Order) #== (lackRelease!Order), tmp#==(re!Prio)))
				container.addConstraint(lackRelease, reqt.IfThen((re!Order) #!= (lackRelease!Order), tmp#==0))
				buffer+=tmp
			}
			container.addConstraint(Subdomain(RELEASEPLAN.toString), reqt.SumEq(buffer.toSeq, minimize))
			println("######")
			println("######")
			println("### SOLVING ####")
			println("Releases: "+size)
			setStatus(8)
			
			if(!ConstraintValidation.isConsistent(container)){
				println("Inconsistent!!!!!")
				println(ConstraintValidation.getInconsistent);
				throw new Exception("Inconsistency during final validation of release plan. Inconsistent constraint is: "+ ConstraintValidation.getInconsistent().toString)
				return container.model
			}else{
				reqt.jacop.Settings.defaultVariableSelection = reqt.jacop.SmallestDomain
				println("SOLVING MINIMIZE NORMAL")
				setStatus(9)
				println((new java.util.Date()).toString)
				var solveTime = System.currentTimeMillis()
				var (m : Model, option : Option[Int]) = container.model.impose(container.model.constraints).minimize(minimize)
				println("ValueSelection: " + reqt.jacop.Settings.defaultValueSelection + "\nVariableSelection: " + reqt.jacop.Settings.defaultVariableSelection)
				println((System.currentTimeMillis()-solveTime)/1000+" seconds to find solution.")
				println((new java.util.Date()).toString)
				if(option==None){
					throw new Exception("No solution found! Please go through given values and constraints before attempting a release planning again!")
				}
				//var (m : Model, r : reqt.Result[Any]) = container.model.constraints.solve(reqt.Minimize(minimize), None, None, jacop.Settings.defaultSelect, Some(Seq(minimize)))
				//var r : reqt.Result[Any] = container.model.constraints.value.toSeq.solve(reqt.Minimize(minimize), assignOption = Some(Seq(minimize)))
				//println(r.lastSolution)
				return m
			}
		}else{
			var NINE = Var("9")
			container.addConstraint(Subdomain(RELEASEPLAN.toString), NINE::Interval(9,9))
			for(s <- stakeholders){
				
				for(re <- requirements){
					var urgencySum = new ListBuffer[reqt.Ref[Int]]

					for(r <- releases){
						// TODO: 
						var tmp = Var("TmpRuhe value of "+re.id+" in release "+r.id+" "+s.id)
						var mul = Var("MulRuhe of prio of "+re.id+" and urgency of "+r.id+" "+s.id)
						var mulStake = Var("MulRuheStake of prio of "+s.id+" "+r.id+" "+re.id)
						var releaseMul = Var("MulRuhe of Mul and Release Prio "+re.id+", "+r.id + " " + s.id)
						if(r.id==lackRelease.id){
							container.addConstraint(s, (r!Prio)::Interval(0,0))
						}
						container.addConstraint(s, mul::Interval(0,90))
						container.addConstraint(s, mul#>=0)
						container.addConstraint(s, releaseMul#>=0)
						container.addConstraint(s, mulStake::Interval(0,2000))
						container.addConstraint(s, releaseMul::Interval(0,20000))
						container.addConstraint(s, tmp::Interval(0,20000))
						container.addConstraint(s, reqt.XmulYeqZ((s!re!Prio),(s!re!r!reqt.Urgency), mul))
						container.addConstraint(s, reqt.XmulYeqZ(mul, (s!Prio), mulStake))
						container.addConstraint(s, reqt.XmulYeqZ(mulStake, (r!Prio), releaseMul))
						container.addConstraint(s, (s!re!r!reqt.Urgency)::Interval(0,9))
						container.addConstraint(s, reqt.IfThen((re!Order) #== (r!Order), tmp#==releaseMul))
						container.addConstraint(s, reqt.IfThen((re!Order) #!= (r!Order), tmp#==0))
						buffer+=tmp
						urgencySum+=(s!re!r!reqt.Urgency)
					}
					container.addConstraint(s, reqt.SumEq(urgencySum.toVector, NINE))
				}
			}
			var utility = Var("Utility function")
			buffer+=utility
			
			var utilityVars = new ListBuffer[reqt.Var[String]]()
			
			//TODO:
			for (i <- 1 to 100) {
				var tmp = Var(UtilityVar.getName(i))
				if(vars.contains(tmp)){
					utilityVars+=tmp
					container.addConstraint(Subdomain(RELEASEPLAN.toString), tmp::Interval(-200000,200000))
				}
			}
			println(utilityVars)
			container.addConstraint(Subdomain(RELEASEPLAN.toString), utility::Interval(-20000000,20000000))
			container.addConstraint(Subdomain(RELEASEPLAN.toString), reqt.SumEq(utilityVars, utility))
			container.addConstraint(Subdomain(RELEASEPLAN.toString), reqt.SumEq(buffer.toSeq, minimize))
			println("######")
			println("######")
			println("### SOLVING ####")
			println("Releases: "+size)
			setStatus(8)
			
			if(!ConstraintValidation.isConsistent(container)){
				println("Inconsistent!!!!!")
				println(ConstraintValidation.getInconsistent);
				throw new Exception("Inconsistency during final validation of release plan. Inconsistent constraint is: "+ ConstraintValidation.getInconsistent().toString)
				return container.model
			}else{
				reqt.jacop.Settings.defaultValueSelection = reqt.jacop.IndomainMin
				reqt.jacop.Settings.defaultVariableSelection = reqt.jacop.SmallestDomain

				println("SOLVING MAXIMIZE RUHE")
				setStatus(9)
				println((new java.util.Date()).toString)
				var (m : Model, option : Option[Int]) = container.model.impose(container.model.constraints).maximize(minimize)
				println("ValueSelection: " + reqt.jacop.Settings.defaultValueSelection + "\nVariableSelection: " + reqt.jacop.Settings.defaultVariableSelection)
				println((new java.util.Date()).toString)
				if(option==None){
					throw new Exception("No solution found! Please go through given values and constraints before attempting a release planning again!")
				}
				return m
			}
		}
		
		/*var count = (for(re <- releases) yield Var("Count of "+re.id)).toList
		
		var minimize = count(count.size-1) //TODO: Dependent on order of releases. We can't be certain of position of "Lack of release" currently.
		println("######")
		println("######")
		println("Minimizing: "+minimize.toString)
		
		for(j <- 0 until releases.size){
			var buffer = new ListBuffer[Var[Any]]()
			println("Counting release: "+releases(j).id)
			for(re <- requirements){
				var tmp = Var("Prio of "+re.id+" in "+releases(j).id)
				container.addConstraint(Subdomain(RELEASEPLAN.toString), reqt.IfThen((re!Order) #== (releases(j)!Order), (re!Prio)#==tmp))
				tmp::Interval(0,reqt.jacop.Settings.defaultInterval.max)
				buffer+=tmp
			}
			container.addConstraint(Subdomain(RELEASEPLAN.toString), reqt.SumEq(buffer.toSeq, count(j)))
		}*/
	}
	
	/**releasePlanning is the main releasePlanning function. Given a container it creates a complete release plan. Creates a new modelTab with the generated solution.
	 * The created release plan is created as a new Model
	 * @param container is the ModelContainer which will be modified and used as basis for the constraints.
	 * @param notUsedMethods contains prioMethods to ignore for each stakeholder
	 * @param res contains the releases that should be included in the release plan
	 * @param isRuhe determines if the plan should be generated with Ruhe's urgency
	 */
	def releasePlanning(container : ModelContainer, notUsedMethods : HashMap[String, HashSet[String]], res : List[Entity], isRuhe : Boolean) {
		STATUS = 1
		println((new java.util.Date()).toString)
		var releases = new ListBuffer[Entity]()
		if(res.size==0){
			throw new Exception("There needs to be at least one release in a release plan!")
		}
		if((container.model / Resource).entities.size==0){
			throw new Exception("There needs to be at least one resource in a release plan!")
		}
		
		releases = releases++res
		
		var tmpContainer = new ModelContainer(container.model)
		container.removePathAttr((Subdomain(PRIORITIZATIONS.toString)!reqt.Constraints.asInstanceOf[AttributeKind[Any]]))
		container.removePathAttr((Subdomain(RELEASEPLAN.toString)!reqt.Constraints.asInstanceOf[AttributeKind[Any]]))
		tmpContainer.setPrioritization(Model())
		reqt.jacop.Settings.defaultInterval = releaseInterval
		
		
		var userConstraints = container.model.attributeValueMap(Constraints)
		println("##### PRIORITIZATION!")
		STATUS = 2
		PrioritizationGeneration.getPrioritization(tmpContainer, notUsedMethods)
		println("##### RELEASE PLANNING!")
		STATUS = 3
		var m = ReleasePlanGeneration.getReleasePlanning(tmpContainer, releases, isRuhe)
		m -= Constraints
		m -= Release(NORELEASE.toString)
		println("### SOLVED ####")
		STATUS = 10
		var mc = container.openModel("Release plan "+(new SimpleDateFormat("dd/MM HH:mm")).format(new java.util.Date()), m)
		STATUS = 11
		if(mc!=null){
			userConstraints.foreach{map =>
				mc.addConstraints(map._1, map._2)
			}
			for(req <- (mc.model / Requirement).entities){
				for(release <- (mc.model / Release / Order).entities){
					if(release.id!=NORELEASE.toString&&((mc.model / req)!!Order) == ((mc.model / release)!!Order)){
						mc.addSubmodelEntity(release, req)
					}
				}
			}
			mc.launch(new ReleaseBoard(mc))
		}
		STATUS = 0
	}
}

/** CreateConstraints create the constraints of all methods in a given container **/
object CreateConstraints{
	
	var ruhe = (new RuheConstraints()).toString
	var dollar = (new ExactDollarConstraints()).toString
	
	var map = new HashMap[String, ConstraintCreator]
	map.put((new ExactDollarConstraints()).toString, new ExactDollarConstraints())
	map.put((new OrdinalConstraints()).toString, new OrdinalConstraints())
	map.put(ruhe, new RuheConstraints())
	
	/** generateConstraints creates the constraints of all methods in a given container
	 * 
	 **/
	def generatePrioritizationConstraints(container : ModelContainer, notUsedMethods : HashMap[String, HashSet[String]]): Boolean = {
		reqt.warn("Generating method constraints")

		var isRuhe = false
		for(s <- (container.model / Stakeholder).entities){
			if(container.model / s !! Prio != 0){
				var sub = (((container.model / s) !! Submodel) / Subdomain("Prio")) !! Submodel
				for(domain <- (sub / Subdomain).entities){
					if (notUsedMethods == null || !(notUsedMethods.containsKey(s.id)&&notUsedMethods.get(s.id).contains(domain.id))) {
						if(map.containsKey(domain.id)){
							map.get(domain.id).createConstraints(container, s, (sub / domain) !! Submodel)
						} else {
							container.addConstraints(s, (((((container.model / s) !! Submodel) / Subdomain("Prio")) !! Submodel) / domain) !! Constraints)							
						}
						if(domain.id==ruhe || domain.id==dollar){
							isRuhe = true
						}
						if(!ConstraintValidation.isConsistent(container)){
							throw new Exception(domain.id+" caused inconsistency for requirement "+ ConstraintValidation.getInconsistent() + " and stakeholder " +s.id+" during prioritization!")
						}
					}
				}
			}
		}
		return isRuhe
	}
}

/** The prioritization method constraints specified by Ruhe in 2005. **/
class RuheConstraints extends ConstraintCreator(){
	/** createConstraints creates the Ruhe constraints of a given Model **/
	override def createConstraints(container : ModelContainer, stakeholder : Entity, model : Model) {
		for (entity <- (model / Requirement).entities) {
			if((model/entity/Prio).entities.contains(entity)){
				container.addConstraint(stakeholder, reqt.XeqC((stakeholder!entity!Prio), (model/entity)!!Prio))
			}
			for(r <- (((model / entity)!!Submodel)/reqt.Urgency).entities){
				container.addConstraint(stakeholder, reqt.XeqC((stakeholder!entity!r!reqt.Urgency), (((model/entity)!!Submodel)/r)!!reqt.Urgency))
			}
		}
		
	}
	override def toString() : String = {return RUHE.toString}
}

/** Automaticly generated utility vars **/
object UtilityVar {
	var nbr = 0
	def getNbr(): Int = {
		nbr+=1
		return nbr
	}
	def getName(value : Int) : String = "Utility function#"+value
	def apply() = new Var(getName(getNbr()))
}

class ExactDollarConstraints extends ConstraintCreator(){
	/** createConstraints creates demands exact $100 constraints of a given Model **/
	override def createConstraints(container : ModelContainer, stakeholder : Entity, model : Model) {
		for (entity <- (model / Requirement / Prio).entities) {
			container.addConstraint(stakeholder, reqt.XeqC((stakeholder!entity!Prio), (model / entity)!!Prio))
		}
	}
	override def toString() : String = {return DOLLAR.toString}
}

/** DollarConstraints which creates constraints for $100 problems **/
class DollarConstraints extends ConstraintCreator(){
	/** createConstraints creates the $100 constraints of a given Model **/
	override def createConstraints(container : ModelContainer, stakeholder : Entity, model : Model) {
		var list = new ArrayList[Sortable]()
		for (entity <- (model / Requirement / Prio).entities) {
			list.add(new Sortable(entity, (model / entity)!!Prio))
		}
		Collections.sort(list)
		var equals = new ArrayList[Sortable]()
		var oldEquals = new ArrayList[Sortable]()
		var current: Sortable = null
		for (s <- list) {
			if (current == null) {
				current = s
			}
			if (current.value != s.value) {
				for (e <- equals) {
					for (o <- oldEquals) {
						container.addConstraint(stakeholder, reqt.XgtY((stakeholder!e.o.asInstanceOf[Entity]!Prio), (stakeholder!o.o.asInstanceOf[Entity]!Prio)))
					}
				}
				oldEquals = equals
				equals = new ArrayList[Sortable]()
			}
			equals.add(s)
			current = s
		}
		for (e <- equals) {
			for (o <- oldEquals) {
				container.addConstraint(stakeholder, reqt.XgtY((stakeholder!e.o.asInstanceOf[Entity]!Prio), (stakeholder!o.o.asInstanceOf[Entity]!Prio)))
			}
		}
	}
	override def toString() : String = {return "OriginalDollarMethod"}
}
/** OrdinalConstraints which creates constraints for ranking problems **/
class OrdinalConstraints extends DollarConstraints{
	override def toString() : String = {return ORDINAL.toString}
}

/** ConstraintCreator creates Constraints **/
class ConstraintCreator(){
	/** createConstraints adds the prioritization constraints to the modelcontainer.
	 * @param container is the ModelContainer that is to be edited.
	 * @param stakeholder is the Entity which prioritizes.
	 * @param model is the stakeholder's current prioritization.
	 */
	def createConstraints(container : ModelContainer, stakeholder : Entity, model : Model) {}
	/** toString returns the name of the ConstraintCreator method.
	 * @return String of the method's name.
	 */
	override def toString() : String = {return "Error unnamed value!"}
}

/** ConstraintValidation validates the constraints of a modelContainer */
object ConstraintValidation {
	
	import JaCoP. { search => jsearch, core => jcore, constraints => jcon }  

   	var recentlyFailed : String = ""

    /**
     * isConsistent makes a quick check to see if any inconsistencies are found, no guarantees if return value is correct.
     * @param container Contains the model with constraints to check
     * @return true if assumed to be consistent
     */
	def isConsistent(container: ModelContainer) :Boolean = {
		val m = container.model
		val constraints: Seq[Constr[Any]] = m.impose(m.constraintsAll).cs

        val solver = new reqt.jacop.Solver[Any](constraints, reqt.jacop.Settings.defaultSearchType, None, None, reqt.jacop.Settings.defaultValueSelection, reqt.jacop.Settings.defaultVariableSelection, None)

		var store = new jcore.Store
        val vs = solver.distinctVars(constraints)
        val cs = solver.collectConstr(constraints)        

        val intVarMap: Map[Var[Any], JaCoP.core.IntVar] = vs.map { v => (v, solver.varToJIntVar(v, store)) } .toMap

        cs foreach { c => store.impose(solver.toJCon(c, store, intVarMap)) }
        var cons = store.consistency()
        if (!cons) {
        	recentlyFailed = store.recentlyFailedConstraint.toString()
        } else {
        	recentlyFailed = ""
        }
        return cons
	}
	/** Return string of most recent failed constraint **/
	def getInconsistent() :String={
		return recentlyFailed
	}
}


