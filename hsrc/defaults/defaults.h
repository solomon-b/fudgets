{-
HBC uses "cpp -C -traditional" which causes all the /**/ to be left behind
when the macro definitions are processed. That is why the definitions
are inside a comment.

#define parameter_class(name,type) parameter_class_gen(name,name,type)

#define parameter_class_gen(classname,par,type) \
class Has/**/classname xxx where {\
    set/**/par :: (type) -> Customiser xxx; \
    get/**/par :: xxx -> (type); \
    get/**/par/**/Maybe :: xxx -> Maybe (type); \
    get/**/par = fromMaybe (error "get par: missing default") . get/**/par/**/Maybe }

#define parameter_class1(par) class Has/**/par xxx where {\
   set/**/par :: a -> Customiser (xxx a); \
   get/**/par/**/Maybe :: xxx a -> Maybe a; \
   get/**/par :: xxx a -> a; \
   get/**/par = fromMaybe (error "get par: missing default") . get/**/par/**/Maybe }

#define parameter_instance1(par,type) parameter_instance(par,type a)

#define parameter_instance(par,type) instance Has/**/par (type) where {\
  set/**/par p (Pars ps) = Pars (par p:ps); \
  get/**/par/**/Maybe (Pars ps) = getparMaybe (\x->case x of par p -> Just p; _-> Nothing) ps }

/* non-overloaded parameters: */
#define parameter(par) \
set/**/par p = cust (\ (Pars ps) -> Pars (par p:ps)); \
get/**/par (Pars ps) = getpar (\x->case x of par p -> Just p; _-> Nothing) ps; \
get/**/par/**/Maybe (Pars ps) = getparMaybe (\x->case x of par p -> Just p; _-> Nothing) ps
  
-}
