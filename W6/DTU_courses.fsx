type CourseNo = int
type Title = string
type ECTS = int
type CourseDesc = Title * ECTS
type CourseBase = Map<CourseNo, CourseDesc>
type Mandatory = Set<CourseNo>
type Optional = Set<CourseNo>
type CourseGroup = Mandatory * Optional

type BasicNaturalScience = CourseGroup
type TechnologicalCore = CourseGroup
type ProjectProfessionalSkill = CourseGroup
type Elective = CourseNo -> bool
type FlagModel = BasicNaturalScience * TechnologicalCore* ProjectProfessionalSkill * Elective
type CoursePlan = Set<CourseNo>

let ex:CourseBase = Map.ofList [(1001,("Course 1", 10)); (1002,("Course 2", 15)); (1003,("Course 3", 5))];;
let cs = Set.ofList [1001;1002]

let isValidCourseDesc (desc: CourseDesc) =
    match desc with
    | (_,e) -> if e%5=0 then true else false;;


let isValidCourseBase (cb: CourseBase) = Map.forall (fun _ desc -> isValidCourseDesc desc) cb


let disjoint s1 s2 = if Set.isSubset s1 s2 || Set.isSubset s2 s1 then false else true


let sumECTS (cs: Set<CourseNo>) (cb: CourseBase) = 
   let newMap = Map.filter (fun key _ -> Set.contains key cs) cb
   Map.foldBack (fun key (cn,e)  state -> state+e) newMap 0;;


let isValideCourseGroup ((man: Mandatory), (opt: Optional)) cb = 
    if sumECTS man cb = 45 && Set.isEmpty opt then
        if disjoint man opt then true 
        else false
    else 
        if disjoint man opt && sumECTS man cb < 45 && sumECTS man cb + sumECTS opt cb >= 45 then true
        else false;;



let isElective (cg: CourseGroup) (ep: Elective) = 
    match cg with 
    | (man, opt) -> if Set.forall ep man && Set.forall ep opt then true else false;;

let DifferentCourses (bns: BasicNaturalScience) (tc: TechnologicalCore) (pps: ProjectProfessionalSkill) =
    match bns with 
    | (bns_m, bns_o) -> match tc with
                        | (tc_m, tc_o) -> match pps with
                                          | (pps_m, pps_o) ->
                                          if disjoint bns_m tc_m && disjoint bns_o tc_o && disjoint bns_m tc_o 
                                          && disjoint bns_m pps_m && disjoint bns_o pps_o && disjoint bns_m pps_o
                                          && disjoint tc_m pps_m && disjoint tc_o pps_o && disjoint tc_m pps_o then true
                                            else false;;

let isValid ((bns, tc, pps, ep): FlagModel) (cb: CourseBase) =
    if isElective bns ep && isElective tc ep && isElective pps ep && isValideCourseGroup bns cb 
    && isValideCourseGroup tc cb && isValideCourseGroup pps cb && DifferentCourses bns tc pps then true 
        else false;;


//let checkPlan (cs: CoursePlan), (cb: CourseBase) =



