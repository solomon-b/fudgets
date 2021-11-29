#if defined(__HBC__) || defined(__GLASGOW_HASKELL__) || defined(__NHC__) || defined(__PFE__)
#define USE_EXIST_Q
#else
#warning No existential types! The type Gfx will be omitted and the function g will be less general.
#endif

#ifdef USE_EXIST_Q
#define IFNOEXIST(e)
#define IFEXIST(e) e
#else
#define IFNOEXIST(e) e
#define IFEXIST(e)
#endif

-- Syntax for existential quantification varies:
#if defined(__HBC__)
#define EQV(x) ?x
#define EXISTS(x) 
#elif defined(__GLASGOW_HASKELL__) || defined(__PFE__)
#define EQV(x) x
#define EXISTS(x) forall x .
#else
-- assume any free type variable is existential (like in NHC and old HBC)
#define EQV(x) x
#define EXISTS(x) 
#endif


#ifdef __PFE__
#define TSTHACK(e) e
#else
#define TSTHACK(e) e
#endif
