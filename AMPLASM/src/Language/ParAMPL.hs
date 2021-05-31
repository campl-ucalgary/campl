{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Language.ParAMPL where
import Language.AbsAMPL
import Language.LexAMPL
import Language.ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (String)
	| HappyAbsSyn5 (Integer)
	| HappyAbsSyn6 (Store)
	| HappyAbsSyn7 (Load)
	| HappyAbsSyn8 (Ret)
	| HappyAbsSyn9 (Call)
	| HappyAbsSyn10 (ConstInt)
	| HappyAbsSyn11 (ConstChar)
	| HappyAbsSyn12 (ConstString)
	| HappyAbsSyn13 (ToStr)
	| HappyAbsSyn14 (ToInt)
	| HappyAbsSyn15 (And)
	| HappyAbsSyn16 (Or)
	| HappyAbsSyn17 (Append)
	| HappyAbsSyn18 (Unstring)
	| HappyAbsSyn19 (LeqI)
	| HappyAbsSyn20 (EqI)
	| HappyAbsSyn21 (Leqc)
	| HappyAbsSyn22 (Eqc)
	| HappyAbsSyn23 (Leqs)
	| HappyAbsSyn24 (Eqs)
	| HappyAbsSyn25 (ConcatS)
	| HappyAbsSyn26 (Add)
	| HappyAbsSyn27 (Subtract)
	| HappyAbsSyn28 (Mul)
	| HappyAbsSyn29 (Quot)
	| HappyAbsSyn30 (Rem)
	| HappyAbsSyn31 (Cons)
	| HappyAbsSyn32 (Case)
	| HappyAbsSyn33 (If)
	| HappyAbsSyn34 (Rec)
	| HappyAbsSyn35 (Get)
	| HappyAbsSyn36 (Put)
	| HappyAbsSyn37 (Hput)
	| HappyAbsSyn38 (Hcase)
	| HappyAbsSyn39 (Split)
	| HappyAbsSyn40 (Fork)
	| HappyAbsSyn41 (Plug)
	| HappyAbsSyn42 (Run)
	| HappyAbsSyn43 (Race)
	| HappyAbsSyn44 (Close)
	| HappyAbsSyn45 (Halt)
	| HappyAbsSyn46 (Ch_Id)
	| HappyAbsSyn47 (Main_run)
	| HappyAbsSyn48 (BTrue)
	| HappyAbsSyn49 (BFalse)
	| HappyAbsSyn50 (Character)
	| HappyAbsSyn51 (UIdent)
	| HappyAbsSyn52 (PIdent)
	| HappyAbsSyn53 (PInteger)
	| HappyAbsSyn54 (IIdent)
	| HappyAbsSyn55 (AMPLCODE)
	| HappyAbsSyn56 (AMPL_CONSTRUCTS)
	| HappyAbsSyn57 ([AMPL_CONSTRUCTS])
	| HappyAbsSyn58 (HANDLE_SPEC)
	| HappyAbsSyn59 (Handle)
	| HappyAbsSyn60 ([HANDLE_SPEC])
	| HappyAbsSyn61 ([Handle])
	| HappyAbsSyn62 (IMPORT)
	| HappyAbsSyn63 (CONSTRUCTORS)
	| HappyAbsSyn64 (DESTRUCTORS)
	| HappyAbsSyn65 (STRUCTOR_SPEC)
	| HappyAbsSyn66 (STRUCT)
	| HappyAbsSyn67 ([STRUCTOR_SPEC])
	| HappyAbsSyn68 ([STRUCT])
	| HappyAbsSyn69 (HANDLES)
	| HappyAbsSyn70 (COHANDLES)
	| HappyAbsSyn71 (PROCESSES)
	| HappyAbsSyn72 ([PROCESS_SPEC])
	| HappyAbsSyn73 (PROCESS_SPEC)
	| HappyAbsSyn74 (Vars)
	| HappyAbsSyn75 ([Vars])
	| HappyAbsSyn76 (FUNCTIONS)
	| HappyAbsSyn77 ([FUNCTION_SPEC])
	| HappyAbsSyn78 (FUNCTION_SPEC)
	| HappyAbsSyn79 (START)
	| HappyAbsSyn80 (CHANNEL_SPEC)
	| HappyAbsSyn81 (COMS)
	| HappyAbsSyn82 ([COM])
	| HappyAbsSyn83 (COM)
	| HappyAbsSyn84 (LABELCOMS)
	| HappyAbsSyn85 ([COMS])
	| HappyAbsSyn86 ([LABELCOMS])
	| HappyAbsSyn87 (RACES)
	| HappyAbsSyn88 ([RACES])
	| HappyAbsSyn89 ([PIdent])
	| HappyAbsSyn90 (CInteger)
	| HappyAbsSyn91 ([CInteger])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201,
 action_202,
 action_203,
 action_204,
 action_205,
 action_206,
 action_207,
 action_208,
 action_209,
 action_210,
 action_211,
 action_212,
 action_213,
 action_214,
 action_215,
 action_216,
 action_217,
 action_218,
 action_219,
 action_220,
 action_221,
 action_222,
 action_223,
 action_224,
 action_225,
 action_226,
 action_227,
 action_228,
 action_229,
 action_230,
 action_231,
 action_232,
 action_233,
 action_234,
 action_235,
 action_236,
 action_237,
 action_238,
 action_239,
 action_240,
 action_241,
 action_242,
 action_243,
 action_244,
 action_245,
 action_246,
 action_247,
 action_248,
 action_249,
 action_250,
 action_251,
 action_252,
 action_253,
 action_254,
 action_255,
 action_256,
 action_257,
 action_258,
 action_259,
 action_260,
 action_261,
 action_262,
 action_263,
 action_264,
 action_265,
 action_266,
 action_267,
 action_268,
 action_269,
 action_270,
 action_271,
 action_272,
 action_273,
 action_274,
 action_275,
 action_276,
 action_277,
 action_278,
 action_279,
 action_280,
 action_281,
 action_282,
 action_283,
 action_284,
 action_285,
 action_286,
 action_287,
 action_288,
 action_289,
 action_290,
 action_291,
 action_292,
 action_293,
 action_294,
 action_295,
 action_296,
 action_297,
 action_298,
 action_299,
 action_300,
 action_301,
 action_302,
 action_303,
 action_304,
 action_305,
 action_306,
 action_307,
 action_308,
 action_309,
 action_310,
 action_311,
 action_312,
 action_313,
 action_314,
 action_315,
 action_316,
 action_317,
 action_318,
 action_319,
 action_320,
 action_321,
 action_322,
 action_323,
 action_324,
 action_325,
 action_326,
 action_327,
 action_328,
 action_329,
 action_330,
 action_331,
 action_332,
 action_333,
 action_334,
 action_335,
 action_336,
 action_337,
 action_338,
 action_339,
 action_340,
 action_341,
 action_342,
 action_343,
 action_344,
 action_345 :: () => Prelude.Int -> ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103,
 happyReduce_104,
 happyReduce_105,
 happyReduce_106,
 happyReduce_107,
 happyReduce_108,
 happyReduce_109,
 happyReduce_110,
 happyReduce_111,
 happyReduce_112,
 happyReduce_113,
 happyReduce_114,
 happyReduce_115,
 happyReduce_116,
 happyReduce_117,
 happyReduce_118,
 happyReduce_119,
 happyReduce_120,
 happyReduce_121,
 happyReduce_122,
 happyReduce_123,
 happyReduce_124,
 happyReduce_125,
 happyReduce_126,
 happyReduce_127,
 happyReduce_128,
 happyReduce_129,
 happyReduce_130,
 happyReduce_131,
 happyReduce_132,
 happyReduce_133,
 happyReduce_134,
 happyReduce_135,
 happyReduce_136,
 happyReduce_137,
 happyReduce_138,
 happyReduce_139,
 happyReduce_140,
 happyReduce_141,
 happyReduce_142,
 happyReduce_143,
 happyReduce_144,
 happyReduce_145,
 happyReduce_146,
 happyReduce_147,
 happyReduce_148,
 happyReduce_149,
 happyReduce_150,
 happyReduce_151,
 happyReduce_152,
 happyReduce_153,
 happyReduce_154,
 happyReduce_155,
 happyReduce_156,
 happyReduce_157,
 happyReduce_158,
 happyReduce_159,
 happyReduce_160,
 happyReduce_161,
 happyReduce_162,
 happyReduce_163,
 happyReduce_164,
 happyReduce_165,
 happyReduce_166,
 happyReduce_167,
 happyReduce_168,
 happyReduce_169,
 happyReduce_170,
 happyReduce_171,
 happyReduce_172 :: () => ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,598) ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2032,0,0,0,8192,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32896,0,65344,65535,65535,108,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,16384,0,0,0,0,8,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,16,0,0,16384,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,514,0,65533,65535,46079,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,2048,8,62464,65535,65535,1743,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,2,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,130,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pAMPLCODE","String","Integer","Store","Load","Ret","Call","ConstInt","ConstChar","ConstString","ToStr","ToInt","And","Or","Append","Unstring","LeqI","EqI","Leqc","Eqc","Leqs","Eqs","ConcatS","Add","Subtract","Mul","Quot","Rem","Cons","Case","If","Rec","Get","Put","Hput","Hcase","Split","Fork","Plug","Run","Race","Close","Halt","Ch_Id","Main_run","BTrue","BFalse","Character","UIdent","PIdent","PInteger","IIdent","AMPLCODE","AMPL_CONSTRUCTS","ListAMPL_CONSTRUCTS","HANDLE_SPEC","Handle","ListHANDLE_SPEC","ListHandle","IMPORT","CONSTRUCTORS","DESTRUCTORS","STRUCTOR_SPEC","STRUCT","ListSTRUCTOR_SPEC","ListSTRUCT","HANDLES","COHANDLES","PROCESSES","ListPROCESS_SPEC","PROCESS_SPEC","Vars","ListVars","FUNCTIONS","ListFUNCTION_SPEC","FUNCTION_SPEC","START","CHANNEL_SPEC","COMS","ListCOM","COM","LABELCOMS","ListCOMS","ListLABELCOMS","RACES","ListRACES","ListPIdent","CInteger","ListCInteger","'#'","'%cohandles'","'%constructors'","'%destructors'","'%functions'","'%handles'","'%include'","'%processes'","'('","')'","','","'-'","'->'","'.'","':'","':='","';'","'='","'=>'","'['","']'","'as'","'else'","'into'","'of'","'on'","'then'","'with'","'{'","'|'","'}'","L_quoted","L_integ","L_Store","L_Load","L_Ret","L_Call","L_ConstInt","L_ConstChar","L_ConstString","L_ToStr","L_ToInt","L_And","L_Or","L_Append","L_Unstring","L_LeqI","L_EqI","L_Leqc","L_Eqc","L_Leqs","L_Eqs","L_ConcatS","L_Add","L_Subtract","L_Mul","L_Quot","L_Rem","L_Cons","L_Case","L_If","L_Rec","L_Get","L_Put","L_Hput","L_Hcase","L_Split","L_Fork","L_Plug","L_Run","L_Race","L_Close","L_Halt","L_Ch_Id","L_Main_run","L_BTrue","L_BFalse","L_Character","L_UIdent","L_PIdent","L_PInteger","L_IIdent","%eof"]
        bit_start = st Prelude.* 174
        bit_end = (st Prelude.+ 1) Prelude.* 174
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..173]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (55) = happyGoto action_3
action_0 (57) = happyGoto action_4
action_0 _ = happyReduce_60

action_1 (123) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (174) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (93) = happyShift action_15
action_4 (94) = happyShift action_16
action_4 (95) = happyShift action_17
action_4 (96) = happyShift action_18
action_4 (97) = happyShift action_19
action_4 (98) = happyShift action_20
action_4 (99) = happyShift action_21
action_4 (166) = happyShift action_22
action_4 (47) = happyGoto action_5
action_4 (56) = happyGoto action_6
action_4 (62) = happyGoto action_7
action_4 (63) = happyGoto action_8
action_4 (64) = happyGoto action_9
action_4 (69) = happyGoto action_10
action_4 (70) = happyGoto action_11
action_4 (71) = happyGoto action_12
action_4 (76) = happyGoto action_13
action_4 (79) = happyGoto action_14
action_4 _ = happyReduce_96

action_5 (100) = happyShift action_32
action_5 (80) = happyGoto action_31
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_61

action_7 _ = happyReduce_53

action_8 _ = happyReduce_56

action_9 _ = happyReduce_57

action_10 _ = happyReduce_54

action_11 _ = happyReduce_55

action_12 _ = happyReduce_58

action_13 _ = happyReduce_59

action_14 _ = happyReduce_52

action_15 (106) = happyShift action_30
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (106) = happyShift action_29
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (106) = happyShift action_28
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (106) = happyShift action_27
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (106) = happyShift action_26
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (173) = happyShift action_25
action_20 (54) = happyGoto action_24
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (106) = happyShift action_23
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_44

action_23 (120) = happyShift action_40
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_69

action_25 _ = happyReduce_51

action_26 (120) = happyShift action_39
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (120) = happyShift action_38
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (120) = happyShift action_37
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (120) = happyShift action_36
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (120) = happyShift action_35
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (106) = happyShift action_34
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (121) = happyShift action_33
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (171) = happyShift action_44
action_33 (52) = happyGoto action_59
action_33 (89) = happyGoto action_60
action_33 _ = happyReduce_165

action_34 (120) = happyShift action_58
action_34 (81) = happyGoto action_57
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (170) = happyShift action_48
action_35 (51) = happyGoto action_45
action_35 (58) = happyGoto action_46
action_35 (60) = happyGoto action_56
action_35 _ = happyReduce_64

action_36 (170) = happyShift action_48
action_36 (51) = happyGoto action_52
action_36 (65) = happyGoto action_53
action_36 (67) = happyGoto action_55
action_36 _ = happyReduce_74

action_37 (170) = happyShift action_48
action_37 (51) = happyGoto action_52
action_37 (65) = happyGoto action_53
action_37 (67) = happyGoto action_54
action_37 _ = happyReduce_74

action_38 (171) = happyShift action_44
action_38 (52) = happyGoto action_49
action_38 (77) = happyGoto action_50
action_38 (78) = happyGoto action_51
action_38 _ = happyReduce_91

action_39 (170) = happyShift action_48
action_39 (51) = happyGoto action_45
action_39 (58) = happyGoto action_46
action_39 (60) = happyGoto action_47
action_39 _ = happyReduce_64

action_40 (171) = happyShift action_44
action_40 (52) = happyGoto action_41
action_40 (72) = happyGoto action_42
action_40 (73) = happyGoto action_43
action_40 _ = happyReduce_82

action_41 (100) = happyShift action_167
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (122) = happyShift action_166
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (108) = happyShift action_165
action_43 _ = happyReduce_83

action_44 _ = happyReduce_49

action_45 (109) = happyShift action_164
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (108) = happyShift action_163
action_46 _ = happyReduce_65

action_47 (122) = happyShift action_162
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_48

action_49 (100) = happyShift action_161
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (122) = happyShift action_160
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (108) = happyShift action_159
action_51 _ = happyReduce_92

action_52 (109) = happyShift action_158
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (108) = happyShift action_157
action_53 _ = happyReduce_75

action_54 (122) = happyShift action_156
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (122) = happyShift action_155
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (122) = happyShift action_154
action_56 _ = happyFail (happyExpListPerState 56)

action_57 _ = happyReduce_95

action_58 (92) = happyShift action_110
action_58 (100) = happyShift action_111
action_58 (123) = happyShift action_2
action_58 (125) = happyShift action_112
action_58 (126) = happyShift action_113
action_58 (127) = happyShift action_114
action_58 (128) = happyShift action_115
action_58 (129) = happyShift action_116
action_58 (130) = happyShift action_117
action_58 (131) = happyShift action_118
action_58 (132) = happyShift action_119
action_58 (133) = happyShift action_120
action_58 (134) = happyShift action_121
action_58 (135) = happyShift action_122
action_58 (136) = happyShift action_123
action_58 (137) = happyShift action_124
action_58 (138) = happyShift action_125
action_58 (139) = happyShift action_126
action_58 (140) = happyShift action_127
action_58 (141) = happyShift action_128
action_58 (142) = happyShift action_129
action_58 (143) = happyShift action_130
action_58 (144) = happyShift action_131
action_58 (145) = happyShift action_132
action_58 (146) = happyShift action_133
action_58 (147) = happyShift action_134
action_58 (148) = happyShift action_135
action_58 (149) = happyShift action_136
action_58 (150) = happyShift action_137
action_58 (151) = happyShift action_138
action_58 (152) = happyShift action_139
action_58 (153) = happyShift action_140
action_58 (154) = happyShift action_141
action_58 (155) = happyShift action_142
action_58 (156) = happyShift action_143
action_58 (157) = happyShift action_144
action_58 (158) = happyShift action_145
action_58 (159) = happyShift action_146
action_58 (160) = happyShift action_147
action_58 (161) = happyShift action_148
action_58 (162) = happyShift action_149
action_58 (163) = happyShift action_150
action_58 (164) = happyShift action_151
action_58 (167) = happyShift action_152
action_58 (168) = happyShift action_153
action_58 (170) = happyShift action_48
action_58 (171) = happyShift action_44
action_58 (4) = happyGoto action_63
action_58 (6) = happyGoto action_64
action_58 (7) = happyGoto action_65
action_58 (8) = happyGoto action_66
action_58 (9) = happyGoto action_67
action_58 (10) = happyGoto action_68
action_58 (11) = happyGoto action_69
action_58 (12) = happyGoto action_70
action_58 (13) = happyGoto action_71
action_58 (14) = happyGoto action_72
action_58 (15) = happyGoto action_73
action_58 (16) = happyGoto action_74
action_58 (17) = happyGoto action_75
action_58 (18) = happyGoto action_76
action_58 (19) = happyGoto action_77
action_58 (20) = happyGoto action_78
action_58 (21) = happyGoto action_79
action_58 (22) = happyGoto action_80
action_58 (23) = happyGoto action_81
action_58 (24) = happyGoto action_82
action_58 (25) = happyGoto action_83
action_58 (26) = happyGoto action_84
action_58 (27) = happyGoto action_85
action_58 (28) = happyGoto action_86
action_58 (29) = happyGoto action_87
action_58 (30) = happyGoto action_88
action_58 (31) = happyGoto action_89
action_58 (32) = happyGoto action_90
action_58 (33) = happyGoto action_91
action_58 (34) = happyGoto action_92
action_58 (35) = happyGoto action_93
action_58 (36) = happyGoto action_94
action_58 (37) = happyGoto action_95
action_58 (38) = happyGoto action_96
action_58 (39) = happyGoto action_97
action_58 (40) = happyGoto action_98
action_58 (41) = happyGoto action_99
action_58 (42) = happyGoto action_100
action_58 (43) = happyGoto action_101
action_58 (44) = happyGoto action_102
action_58 (45) = happyGoto action_103
action_58 (48) = happyGoto action_104
action_58 (49) = happyGoto action_105
action_58 (51) = happyGoto action_106
action_58 (52) = happyGoto action_107
action_58 (82) = happyGoto action_108
action_58 (83) = happyGoto action_109
action_58 _ = happyReduce_99

action_59 (102) = happyShift action_62
action_59 _ = happyReduce_166

action_60 (110) = happyShift action_61
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (171) = happyShift action_44
action_61 (52) = happyGoto action_59
action_61 (89) = happyGoto action_214
action_61 _ = happyReduce_165

action_62 (171) = happyShift action_44
action_62 (52) = happyGoto action_59
action_62 (89) = happyGoto action_213
action_62 _ = happyReduce_165

action_63 _ = happyReduce_150

action_64 (171) = happyShift action_44
action_64 (52) = happyGoto action_212
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (171) = happyShift action_44
action_65 (52) = happyGoto action_211
action_65 _ = happyFail (happyExpListPerState 65)

action_66 _ = happyReduce_105

action_67 (171) = happyShift action_44
action_67 (52) = happyGoto action_210
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (103) = happyShift action_209
action_68 (172) = happyShift action_180
action_68 (53) = happyGoto action_207
action_68 (90) = happyGoto action_208
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (169) = happyShift action_206
action_69 (50) = happyGoto action_205
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (123) = happyShift action_2
action_70 (4) = happyGoto action_204
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_110

action_72 _ = happyReduce_111

action_73 _ = happyReduce_112

action_74 _ = happyReduce_113

action_75 _ = happyReduce_114

action_76 _ = happyReduce_117

action_77 _ = happyReduce_118

action_78 _ = happyReduce_119

action_79 _ = happyReduce_120

action_80 _ = happyReduce_121

action_81 _ = happyReduce_122

action_82 _ = happyReduce_123

action_83 (124) = happyShift action_203
action_83 (5) = happyGoto action_202
action_83 _ = happyFail (happyExpListPerState 83)

action_84 _ = happyReduce_125

action_85 _ = happyReduce_126

action_86 _ = happyReduce_127

action_87 _ = happyReduce_128

action_88 _ = happyReduce_129

action_89 (100) = happyShift action_201
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (171) = happyShift action_44
action_90 (52) = happyGoto action_200
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (171) = happyShift action_44
action_91 (52) = happyGoto action_199
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (116) = happyShift action_198
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (171) = happyShift action_44
action_93 (52) = happyGoto action_197
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (171) = happyShift action_44
action_94 (52) = happyGoto action_196
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (170) = happyShift action_48
action_95 (51) = happyGoto action_195
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (171) = happyShift action_44
action_96 (52) = happyGoto action_194
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (171) = happyShift action_44
action_97 (52) = happyGoto action_193
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (171) = happyShift action_44
action_98 (52) = happyGoto action_192
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (171) = happyShift action_44
action_99 (52) = happyGoto action_59
action_99 (89) = happyGoto action_191
action_99 _ = happyReduce_165

action_100 (171) = happyShift action_44
action_100 (52) = happyGoto action_190
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (120) = happyShift action_189
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (171) = happyShift action_44
action_102 (52) = happyGoto action_188
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (171) = happyShift action_44
action_103 (52) = happyGoto action_187
action_103 _ = happyFail (happyExpListPerState 103)

action_104 _ = happyReduce_115

action_105 _ = happyReduce_116

action_106 (105) = happyShift action_186
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (107) = happyShift action_184
action_107 (165) = happyShift action_185
action_107 (46) = happyGoto action_183
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (122) = happyShift action_182
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (108) = happyShift action_181
action_109 _ = happyReduce_100

action_110 (172) = happyShift action_180
action_110 (53) = happyGoto action_179
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (171) = happyShift action_44
action_111 (52) = happyGoto action_59
action_111 (89) = happyGoto action_178
action_111 _ = happyReduce_165

action_112 _ = happyReduce_3

action_113 _ = happyReduce_4

action_114 _ = happyReduce_5

action_115 _ = happyReduce_6

action_116 _ = happyReduce_7

action_117 _ = happyReduce_8

action_118 _ = happyReduce_9

action_119 _ = happyReduce_10

action_120 _ = happyReduce_11

action_121 _ = happyReduce_12

action_122 _ = happyReduce_13

action_123 _ = happyReduce_14

action_124 _ = happyReduce_15

action_125 _ = happyReduce_16

action_126 _ = happyReduce_17

action_127 _ = happyReduce_18

action_128 _ = happyReduce_19

action_129 _ = happyReduce_20

action_130 _ = happyReduce_21

action_131 _ = happyReduce_22

action_132 _ = happyReduce_23

action_133 _ = happyReduce_24

action_134 _ = happyReduce_25

action_135 _ = happyReduce_26

action_136 _ = happyReduce_27

action_137 _ = happyReduce_28

action_138 _ = happyReduce_29

action_139 _ = happyReduce_30

action_140 _ = happyReduce_31

action_141 _ = happyReduce_32

action_142 _ = happyReduce_33

action_143 _ = happyReduce_34

action_144 _ = happyReduce_35

action_145 _ = happyReduce_36

action_146 _ = happyReduce_37

action_147 _ = happyReduce_38

action_148 _ = happyReduce_39

action_149 _ = happyReduce_40

action_150 _ = happyReduce_41

action_151 _ = happyReduce_42

action_152 _ = happyReduce_45

action_153 _ = happyReduce_46

action_154 _ = happyReduce_80

action_155 _ = happyReduce_70

action_156 _ = happyReduce_71

action_157 (170) = happyShift action_48
action_157 (51) = happyGoto action_52
action_157 (65) = happyGoto action_53
action_157 (67) = happyGoto action_177
action_157 _ = happyReduce_74

action_158 (120) = happyShift action_176
action_158 _ = happyFail (happyExpListPerState 158)

action_159 (171) = happyShift action_44
action_159 (52) = happyGoto action_49
action_159 (77) = happyGoto action_175
action_159 (78) = happyGoto action_51
action_159 _ = happyReduce_91

action_160 _ = happyReduce_90

action_161 (171) = happyShift action_44
action_161 (52) = happyGoto action_168
action_161 (74) = happyGoto action_169
action_161 (75) = happyGoto action_174
action_161 _ = happyReduce_87

action_162 _ = happyReduce_79

action_163 (170) = happyShift action_48
action_163 (51) = happyGoto action_45
action_163 (58) = happyGoto action_46
action_163 (60) = happyGoto action_173
action_163 _ = happyReduce_64

action_164 (120) = happyShift action_172
action_164 _ = happyFail (happyExpListPerState 164)

action_165 (171) = happyShift action_44
action_165 (52) = happyGoto action_41
action_165 (72) = happyGoto action_171
action_165 (73) = happyGoto action_43
action_165 _ = happyReduce_82

action_166 _ = happyReduce_81

action_167 (171) = happyShift action_44
action_167 (52) = happyGoto action_168
action_167 (74) = happyGoto action_169
action_167 (75) = happyGoto action_170
action_167 _ = happyReduce_87

action_168 _ = happyReduce_86

action_169 (102) = happyShift action_247
action_169 _ = happyReduce_88

action_170 (121) = happyShift action_246
action_170 _ = happyFail (happyExpListPerState 170)

action_171 _ = happyReduce_84

action_172 (170) = happyShift action_48
action_172 (51) = happyGoto action_243
action_172 (59) = happyGoto action_244
action_172 (61) = happyGoto action_245
action_172 _ = happyFail (happyExpListPerState 172)

action_173 _ = happyReduce_66

action_174 (101) = happyShift action_242
action_174 _ = happyFail (happyExpListPerState 174)

action_175 _ = happyReduce_93

action_176 (170) = happyShift action_48
action_176 (51) = happyGoto action_239
action_176 (66) = happyGoto action_240
action_176 (68) = happyGoto action_241
action_176 _ = happyFail (happyExpListPerState 176)

action_177 _ = happyReduce_76

action_178 (101) = happyShift action_238
action_178 _ = happyFail (happyExpListPerState 178)

action_179 (100) = happyShift action_237
action_179 _ = happyFail (happyExpListPerState 179)

action_180 _ = happyReduce_50

action_181 (92) = happyShift action_110
action_181 (100) = happyShift action_111
action_181 (123) = happyShift action_2
action_181 (125) = happyShift action_112
action_181 (126) = happyShift action_113
action_181 (127) = happyShift action_114
action_181 (128) = happyShift action_115
action_181 (129) = happyShift action_116
action_181 (130) = happyShift action_117
action_181 (131) = happyShift action_118
action_181 (132) = happyShift action_119
action_181 (133) = happyShift action_120
action_181 (134) = happyShift action_121
action_181 (135) = happyShift action_122
action_181 (136) = happyShift action_123
action_181 (137) = happyShift action_124
action_181 (138) = happyShift action_125
action_181 (139) = happyShift action_126
action_181 (140) = happyShift action_127
action_181 (141) = happyShift action_128
action_181 (142) = happyShift action_129
action_181 (143) = happyShift action_130
action_181 (144) = happyShift action_131
action_181 (145) = happyShift action_132
action_181 (146) = happyShift action_133
action_181 (147) = happyShift action_134
action_181 (148) = happyShift action_135
action_181 (149) = happyShift action_136
action_181 (150) = happyShift action_137
action_181 (151) = happyShift action_138
action_181 (152) = happyShift action_139
action_181 (153) = happyShift action_140
action_181 (154) = happyShift action_141
action_181 (155) = happyShift action_142
action_181 (156) = happyShift action_143
action_181 (157) = happyShift action_144
action_181 (158) = happyShift action_145
action_181 (159) = happyShift action_146
action_181 (160) = happyShift action_147
action_181 (161) = happyShift action_148
action_181 (162) = happyShift action_149
action_181 (163) = happyShift action_150
action_181 (164) = happyShift action_151
action_181 (167) = happyShift action_152
action_181 (168) = happyShift action_153
action_181 (170) = happyShift action_48
action_181 (171) = happyShift action_44
action_181 (4) = happyGoto action_63
action_181 (6) = happyGoto action_64
action_181 (7) = happyGoto action_65
action_181 (8) = happyGoto action_66
action_181 (9) = happyGoto action_67
action_181 (10) = happyGoto action_68
action_181 (11) = happyGoto action_69
action_181 (12) = happyGoto action_70
action_181 (13) = happyGoto action_71
action_181 (14) = happyGoto action_72
action_181 (15) = happyGoto action_73
action_181 (16) = happyGoto action_74
action_181 (17) = happyGoto action_75
action_181 (18) = happyGoto action_76
action_181 (19) = happyGoto action_77
action_181 (20) = happyGoto action_78
action_181 (21) = happyGoto action_79
action_181 (22) = happyGoto action_80
action_181 (23) = happyGoto action_81
action_181 (24) = happyGoto action_82
action_181 (25) = happyGoto action_83
action_181 (26) = happyGoto action_84
action_181 (27) = happyGoto action_85
action_181 (28) = happyGoto action_86
action_181 (29) = happyGoto action_87
action_181 (30) = happyGoto action_88
action_181 (31) = happyGoto action_89
action_181 (32) = happyGoto action_90
action_181 (33) = happyGoto action_91
action_181 (34) = happyGoto action_92
action_181 (35) = happyGoto action_93
action_181 (36) = happyGoto action_94
action_181 (37) = happyGoto action_95
action_181 (38) = happyGoto action_96
action_181 (39) = happyGoto action_97
action_181 (40) = happyGoto action_98
action_181 (41) = happyGoto action_99
action_181 (42) = happyGoto action_100
action_181 (43) = happyGoto action_101
action_181 (44) = happyGoto action_102
action_181 (45) = happyGoto action_103
action_181 (48) = happyGoto action_104
action_181 (49) = happyGoto action_105
action_181 (51) = happyGoto action_106
action_181 (52) = happyGoto action_107
action_181 (82) = happyGoto action_236
action_181 (83) = happyGoto action_109
action_181 _ = happyReduce_99

action_182 _ = happyReduce_98

action_183 (171) = happyShift action_44
action_183 (52) = happyGoto action_235
action_183 _ = happyFail (happyExpListPerState 183)

action_184 (92) = happyShift action_110
action_184 (100) = happyShift action_111
action_184 (123) = happyShift action_2
action_184 (125) = happyShift action_112
action_184 (126) = happyShift action_113
action_184 (127) = happyShift action_114
action_184 (128) = happyShift action_115
action_184 (129) = happyShift action_116
action_184 (130) = happyShift action_117
action_184 (131) = happyShift action_118
action_184 (132) = happyShift action_119
action_184 (133) = happyShift action_120
action_184 (134) = happyShift action_121
action_184 (135) = happyShift action_122
action_184 (136) = happyShift action_123
action_184 (137) = happyShift action_124
action_184 (138) = happyShift action_125
action_184 (139) = happyShift action_126
action_184 (140) = happyShift action_127
action_184 (141) = happyShift action_128
action_184 (142) = happyShift action_129
action_184 (143) = happyShift action_130
action_184 (144) = happyShift action_131
action_184 (145) = happyShift action_132
action_184 (146) = happyShift action_133
action_184 (147) = happyShift action_134
action_184 (148) = happyShift action_135
action_184 (149) = happyShift action_136
action_184 (150) = happyShift action_137
action_184 (151) = happyShift action_138
action_184 (152) = happyShift action_139
action_184 (153) = happyShift action_140
action_184 (154) = happyShift action_141
action_184 (155) = happyShift action_142
action_184 (156) = happyShift action_143
action_184 (157) = happyShift action_144
action_184 (158) = happyShift action_145
action_184 (159) = happyShift action_146
action_184 (160) = happyShift action_147
action_184 (161) = happyShift action_148
action_184 (162) = happyShift action_149
action_184 (163) = happyShift action_150
action_184 (164) = happyShift action_151
action_184 (167) = happyShift action_152
action_184 (168) = happyShift action_153
action_184 (170) = happyShift action_48
action_184 (171) = happyShift action_44
action_184 (4) = happyGoto action_63
action_184 (6) = happyGoto action_64
action_184 (7) = happyGoto action_65
action_184 (8) = happyGoto action_66
action_184 (9) = happyGoto action_67
action_184 (10) = happyGoto action_68
action_184 (11) = happyGoto action_69
action_184 (12) = happyGoto action_70
action_184 (13) = happyGoto action_71
action_184 (14) = happyGoto action_72
action_184 (15) = happyGoto action_73
action_184 (16) = happyGoto action_74
action_184 (17) = happyGoto action_75
action_184 (18) = happyGoto action_76
action_184 (19) = happyGoto action_77
action_184 (20) = happyGoto action_78
action_184 (21) = happyGoto action_79
action_184 (22) = happyGoto action_80
action_184 (23) = happyGoto action_81
action_184 (24) = happyGoto action_82
action_184 (25) = happyGoto action_83
action_184 (26) = happyGoto action_84
action_184 (27) = happyGoto action_85
action_184 (28) = happyGoto action_86
action_184 (29) = happyGoto action_87
action_184 (30) = happyGoto action_88
action_184 (31) = happyGoto action_89
action_184 (32) = happyGoto action_90
action_184 (33) = happyGoto action_91
action_184 (34) = happyGoto action_92
action_184 (35) = happyGoto action_93
action_184 (36) = happyGoto action_94
action_184 (37) = happyGoto action_95
action_184 (38) = happyGoto action_96
action_184 (39) = happyGoto action_97
action_184 (40) = happyGoto action_98
action_184 (41) = happyGoto action_99
action_184 (42) = happyGoto action_100
action_184 (43) = happyGoto action_101
action_184 (44) = happyGoto action_102
action_184 (45) = happyGoto action_103
action_184 (48) = happyGoto action_104
action_184 (49) = happyGoto action_105
action_184 (51) = happyGoto action_106
action_184 (52) = happyGoto action_107
action_184 (83) = happyGoto action_234
action_184 _ = happyFail (happyExpListPerState 184)

action_185 _ = happyReduce_43

action_186 (170) = happyShift action_48
action_186 (51) = happyGoto action_233
action_186 _ = happyFail (happyExpListPerState 186)

action_187 _ = happyReduce_152

action_188 _ = happyReduce_151

action_189 (171) = happyShift action_44
action_189 (52) = happyGoto action_230
action_189 (87) = happyGoto action_231
action_189 (88) = happyGoto action_232
action_189 _ = happyReduce_162

action_190 (100) = happyShift action_229
action_190 _ = happyFail (happyExpListPerState 190)

action_191 (113) = happyShift action_228
action_191 _ = happyFail (happyExpListPerState 191)

action_192 (113) = happyShift action_227
action_192 _ = happyFail (happyExpListPerState 192)

action_193 (115) = happyShift action_226
action_193 _ = happyFail (happyExpListPerState 193)

action_194 (116) = happyShift action_225
action_194 _ = happyFail (happyExpListPerState 194)

action_195 (105) = happyShift action_224
action_195 _ = happyFail (happyExpListPerState 195)

action_196 (117) = happyShift action_223
action_196 _ = happyFail (happyExpListPerState 196)

action_197 (117) = happyShift action_222
action_197 _ = happyFail (happyExpListPerState 197)

action_198 (120) = happyShift action_221
action_198 _ = happyFail (happyExpListPerState 198)

action_199 (118) = happyShift action_220
action_199 _ = happyFail (happyExpListPerState 199)

action_200 (116) = happyShift action_219
action_200 _ = happyFail (happyExpListPerState 200)

action_201 (172) = happyShift action_180
action_201 (53) = happyGoto action_218
action_201 _ = happyFail (happyExpListPerState 201)

action_202 _ = happyReduce_124

action_203 _ = happyReduce_2

action_204 _ = happyReduce_109

action_205 _ = happyReduce_108

action_206 _ = happyReduce_47

action_207 _ = happyReduce_168

action_208 _ = happyReduce_107

action_209 (172) = happyShift action_180
action_209 (53) = happyGoto action_217
action_209 _ = happyFail (happyExpListPerState 209)

action_210 (100) = happyShift action_216
action_210 _ = happyFail (happyExpListPerState 210)

action_211 _ = happyReduce_104

action_212 _ = happyReduce_103

action_213 _ = happyReduce_167

action_214 (101) = happyShift action_215
action_214 _ = happyFail (happyExpListPerState 214)

action_215 _ = happyReduce_97

action_216 (171) = happyShift action_44
action_216 (52) = happyGoto action_59
action_216 (89) = happyGoto action_276
action_216 _ = happyReduce_165

action_217 _ = happyReduce_169

action_218 (102) = happyShift action_275
action_218 _ = happyFail (happyExpListPerState 218)

action_219 (120) = happyShift action_274
action_219 _ = happyFail (happyExpListPerState 219)

action_220 (120) = happyShift action_58
action_220 (81) = happyGoto action_273
action_220 _ = happyFail (happyExpListPerState 220)

action_221 (170) = happyShift action_48
action_221 (51) = happyGoto action_270
action_221 (84) = happyGoto action_271
action_221 (86) = happyGoto action_272
action_221 _ = happyReduce_158

action_222 (171) = happyShift action_44
action_222 (52) = happyGoto action_269
action_222 _ = happyFail (happyExpListPerState 222)

action_223 (171) = happyShift action_44
action_223 (52) = happyGoto action_268
action_223 _ = happyFail (happyExpListPerState 223)

action_224 (170) = happyShift action_48
action_224 (51) = happyGoto action_267
action_224 _ = happyFail (happyExpListPerState 224)

action_225 (120) = happyShift action_266
action_225 _ = happyFail (happyExpListPerState 225)

action_226 (171) = happyShift action_44
action_226 (52) = happyGoto action_265
action_226 _ = happyFail (happyExpListPerState 226)

action_227 (120) = happyShift action_264
action_227 _ = happyFail (happyExpListPerState 227)

action_228 (120) = happyShift action_263
action_228 _ = happyFail (happyExpListPerState 228)

action_229 (171) = happyShift action_44
action_229 (52) = happyGoto action_59
action_229 (89) = happyGoto action_262
action_229 _ = happyReduce_165

action_230 (104) = happyShift action_261
action_230 _ = happyFail (happyExpListPerState 230)

action_231 (108) = happyShift action_260
action_231 _ = happyReduce_163

action_232 (122) = happyShift action_259
action_232 _ = happyFail (happyExpListPerState 232)

action_233 (100) = happyShift action_258
action_233 (171) = happyShift action_44
action_233 (52) = happyGoto action_257
action_233 _ = happyReduce_131

action_234 _ = happyReduce_102

action_235 _ = happyReduce_146

action_236 _ = happyReduce_101

action_237 (171) = happyShift action_44
action_237 (52) = happyGoto action_256
action_237 _ = happyFail (happyExpListPerState 237)

action_238 _ = happyReduce_148

action_239 (172) = happyShift action_180
action_239 (53) = happyGoto action_255
action_239 _ = happyFail (happyExpListPerState 239)

action_240 (108) = happyShift action_254
action_240 _ = happyReduce_77

action_241 (122) = happyShift action_253
action_241 _ = happyFail (happyExpListPerState 241)

action_242 (109) = happyShift action_252
action_242 _ = happyFail (happyExpListPerState 242)

action_243 _ = happyReduce_63

action_244 (108) = happyShift action_251
action_244 _ = happyReduce_67

action_245 (122) = happyShift action_250
action_245 _ = happyFail (happyExpListPerState 245)

action_246 (171) = happyShift action_44
action_246 (52) = happyGoto action_59
action_246 (89) = happyGoto action_249
action_246 _ = happyReduce_165

action_247 (171) = happyShift action_44
action_247 (52) = happyGoto action_168
action_247 (74) = happyGoto action_169
action_247 (75) = happyGoto action_248
action_247 _ = happyReduce_87

action_248 _ = happyReduce_89

action_249 (110) = happyShift action_297
action_249 _ = happyFail (happyExpListPerState 249)

action_250 _ = happyReduce_62

action_251 (170) = happyShift action_48
action_251 (51) = happyGoto action_243
action_251 (59) = happyGoto action_244
action_251 (61) = happyGoto action_296
action_251 _ = happyFail (happyExpListPerState 251)

action_252 (120) = happyShift action_58
action_252 (81) = happyGoto action_295
action_252 _ = happyFail (happyExpListPerState 252)

action_253 _ = happyReduce_72

action_254 (170) = happyShift action_48
action_254 (51) = happyGoto action_239
action_254 (66) = happyGoto action_240
action_254 (68) = happyGoto action_294
action_254 _ = happyFail (happyExpListPerState 254)

action_255 _ = happyReduce_73

action_256 (101) = happyShift action_293
action_256 _ = happyFail (happyExpListPerState 256)

action_257 _ = happyReduce_136

action_258 (171) = happyShift action_44
action_258 (52) = happyGoto action_59
action_258 (89) = happyGoto action_292
action_258 _ = happyReduce_165

action_259 _ = happyReduce_147

action_260 (171) = happyShift action_44
action_260 (52) = happyGoto action_230
action_260 (87) = happyGoto action_231
action_260 (88) = happyGoto action_291
action_260 _ = happyReduce_162

action_261 (120) = happyShift action_58
action_261 (81) = happyGoto action_290
action_261 _ = happyFail (happyExpListPerState 261)

action_262 (121) = happyShift action_289
action_262 _ = happyFail (happyExpListPerState 262)

action_263 (119) = happyShift action_288
action_263 _ = happyFail (happyExpListPerState 263)

action_264 (171) = happyShift action_44
action_264 (52) = happyGoto action_287
action_264 _ = happyFail (happyExpListPerState 264)

action_265 (171) = happyShift action_44
action_265 (52) = happyGoto action_286
action_265 _ = happyFail (happyExpListPerState 265)

action_266 (170) = happyShift action_48
action_266 (51) = happyGoto action_270
action_266 (84) = happyGoto action_271
action_266 (86) = happyGoto action_285
action_266 _ = happyReduce_158

action_267 (117) = happyShift action_284
action_267 _ = happyFail (happyExpListPerState 267)

action_268 _ = happyReduce_141

action_269 _ = happyReduce_138

action_270 (105) = happyShift action_283
action_270 _ = happyFail (happyExpListPerState 270)

action_271 (108) = happyShift action_282
action_271 _ = happyReduce_159

action_272 (122) = happyShift action_281
action_272 _ = happyFail (happyExpListPerState 272)

action_273 (114) = happyShift action_280
action_273 _ = happyFail (happyExpListPerState 273)

action_274 (170) = happyShift action_48
action_274 (51) = happyGoto action_270
action_274 (84) = happyGoto action_271
action_274 (86) = happyGoto action_279
action_274 _ = happyReduce_158

action_275 (172) = happyShift action_180
action_275 (53) = happyGoto action_278
action_275 _ = happyFail (happyExpListPerState 275)

action_276 (101) = happyShift action_277
action_276 _ = happyFail (happyExpListPerState 276)

action_277 _ = happyReduce_106

action_278 (101) = happyShift action_309
action_278 _ = happyFail (happyExpListPerState 278)

action_279 (122) = happyShift action_308
action_279 _ = happyFail (happyExpListPerState 279)

action_280 (120) = happyShift action_58
action_280 (81) = happyGoto action_307
action_280 _ = happyFail (happyExpListPerState 280)

action_281 _ = happyReduce_135

action_282 (170) = happyShift action_48
action_282 (51) = happyGoto action_270
action_282 (84) = happyGoto action_271
action_282 (86) = happyGoto action_306
action_282 _ = happyReduce_158

action_283 (170) = happyShift action_48
action_283 (51) = happyGoto action_305
action_283 _ = happyFail (happyExpListPerState 283)

action_284 (171) = happyShift action_44
action_284 (52) = happyGoto action_304
action_284 _ = happyFail (happyExpListPerState 284)

action_285 (122) = happyShift action_303
action_285 _ = happyFail (happyExpListPerState 285)

action_286 _ = happyReduce_142

action_287 (119) = happyShift action_302
action_287 _ = happyFail (happyExpListPerState 287)

action_288 (111) = happyShift action_301
action_288 _ = happyFail (happyExpListPerState 288)

action_289 (171) = happyShift action_44
action_289 (52) = happyGoto action_59
action_289 (89) = happyGoto action_300
action_289 _ = happyReduce_165

action_290 _ = happyReduce_161

action_291 _ = happyReduce_164

action_292 (101) = happyShift action_299
action_292 _ = happyFail (happyExpListPerState 292)

action_293 _ = happyReduce_149

action_294 _ = happyReduce_78

action_295 _ = happyReduce_94

action_296 _ = happyReduce_68

action_297 (171) = happyShift action_44
action_297 (52) = happyGoto action_59
action_297 (89) = happyGoto action_298
action_297 _ = happyReduce_165

action_298 (101) = happyShift action_316
action_298 _ = happyFail (happyExpListPerState 298)

action_299 (171) = happyShift action_44
action_299 (52) = happyGoto action_315
action_299 _ = happyReduce_132

action_300 (110) = happyShift action_314
action_300 _ = happyFail (happyExpListPerState 300)

action_301 (171) = happyShift action_44
action_301 (52) = happyGoto action_59
action_301 (89) = happyGoto action_313
action_301 _ = happyReduce_165

action_302 (171) = happyShift action_44
action_302 (52) = happyGoto action_59
action_302 (89) = happyGoto action_312
action_302 _ = happyReduce_165

action_303 _ = happyReduce_140

action_304 _ = happyReduce_139

action_305 (100) = happyShift action_310
action_305 (106) = happyShift action_311
action_305 _ = happyFail (happyExpListPerState 305)

action_306 _ = happyReduce_160

action_307 _ = happyReduce_134

action_308 _ = happyReduce_133

action_309 _ = happyReduce_130

action_310 (171) = happyShift action_44
action_310 (52) = happyGoto action_59
action_310 (89) = happyGoto action_322
action_310 _ = happyReduce_165

action_311 (120) = happyShift action_58
action_311 (81) = happyGoto action_321
action_311 _ = happyFail (happyExpListPerState 311)

action_312 (106) = happyShift action_320
action_312 _ = happyFail (happyExpListPerState 312)

action_313 (112) = happyShift action_319
action_313 _ = happyFail (happyExpListPerState 313)

action_314 (171) = happyShift action_44
action_314 (52) = happyGoto action_59
action_314 (89) = happyGoto action_318
action_314 _ = happyReduce_165

action_315 _ = happyReduce_137

action_316 (109) = happyShift action_317
action_316 _ = happyFail (happyExpListPerState 316)

action_317 (120) = happyShift action_58
action_317 (81) = happyGoto action_327
action_317 _ = happyFail (happyExpListPerState 317)

action_318 (101) = happyShift action_326
action_318 _ = happyFail (happyExpListPerState 318)

action_319 (106) = happyShift action_325
action_319 _ = happyFail (happyExpListPerState 319)

action_320 (120) = happyShift action_58
action_320 (81) = happyGoto action_324
action_320 _ = happyFail (happyExpListPerState 320)

action_321 _ = happyReduce_153

action_322 (101) = happyShift action_323
action_322 _ = happyFail (happyExpListPerState 322)

action_323 (106) = happyShift action_330
action_323 _ = happyFail (happyExpListPerState 323)

action_324 (108) = happyShift action_329
action_324 _ = happyFail (happyExpListPerState 324)

action_325 (120) = happyShift action_58
action_325 (81) = happyGoto action_328
action_325 _ = happyFail (happyExpListPerState 325)

action_326 _ = happyReduce_145

action_327 _ = happyReduce_85

action_328 (108) = happyShift action_333
action_328 _ = happyFail (happyExpListPerState 328)

action_329 (171) = happyShift action_44
action_329 (52) = happyGoto action_332
action_329 _ = happyFail (happyExpListPerState 329)

action_330 (120) = happyShift action_58
action_330 (81) = happyGoto action_331
action_330 _ = happyFail (happyExpListPerState 330)

action_331 _ = happyReduce_154

action_332 (119) = happyShift action_335
action_332 _ = happyFail (happyExpListPerState 332)

action_333 (119) = happyShift action_334
action_333 _ = happyFail (happyExpListPerState 333)

action_334 (111) = happyShift action_337
action_334 _ = happyFail (happyExpListPerState 334)

action_335 (171) = happyShift action_44
action_335 (52) = happyGoto action_59
action_335 (89) = happyGoto action_336
action_335 _ = happyReduce_165

action_336 (106) = happyShift action_339
action_336 _ = happyFail (happyExpListPerState 336)

action_337 (171) = happyShift action_44
action_337 (52) = happyGoto action_59
action_337 (89) = happyGoto action_338
action_337 _ = happyReduce_165

action_338 (112) = happyShift action_341
action_338 _ = happyFail (happyExpListPerState 338)

action_339 (120) = happyShift action_58
action_339 (81) = happyGoto action_340
action_339 _ = happyFail (happyExpListPerState 339)

action_340 (122) = happyShift action_343
action_340 _ = happyFail (happyExpListPerState 340)

action_341 (106) = happyShift action_342
action_341 _ = happyFail (happyExpListPerState 341)

action_342 (120) = happyShift action_58
action_342 (81) = happyGoto action_344
action_342 _ = happyFail (happyExpListPerState 342)

action_343 _ = happyReduce_143

action_344 (122) = happyShift action_345
action_344 _ = happyFail (happyExpListPerState 344)

action_345 _ = happyReduce_144

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn5
		 ((read ( happy_var_1)) :: Integer
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (Store (mkPosToken happy_var_1)
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (Load (mkPosToken happy_var_1)
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  8 happyReduction_5
happyReduction_5 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (Ret (mkPosToken happy_var_1)
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  9 happyReduction_6
happyReduction_6 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (Call (mkPosToken happy_var_1)
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  10 happyReduction_7
happyReduction_7 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (ConstInt (mkPosToken happy_var_1)
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  11 happyReduction_8
happyReduction_8 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (ConstChar (mkPosToken happy_var_1)
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  12 happyReduction_9
happyReduction_9 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (ConstString (mkPosToken happy_var_1)
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  13 happyReduction_10
happyReduction_10 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn13
		 (ToStr (mkPosToken happy_var_1)
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  14 happyReduction_11
happyReduction_11 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (ToInt (mkPosToken happy_var_1)
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  15 happyReduction_12
happyReduction_12 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn15
		 (And (mkPosToken happy_var_1)
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  16 happyReduction_13
happyReduction_13 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 (Or (mkPosToken happy_var_1)
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  17 happyReduction_14
happyReduction_14 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (Append (mkPosToken happy_var_1)
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  18 happyReduction_15
happyReduction_15 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (Unstring (mkPosToken happy_var_1)
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  19 happyReduction_16
happyReduction_16 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn19
		 (LeqI (mkPosToken happy_var_1)
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  20 happyReduction_17
happyReduction_17 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (EqI (mkPosToken happy_var_1)
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  21 happyReduction_18
happyReduction_18 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 (Leqc (mkPosToken happy_var_1)
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  22 happyReduction_19
happyReduction_19 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (Eqc (mkPosToken happy_var_1)
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  23 happyReduction_20
happyReduction_20 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn23
		 (Leqs (mkPosToken happy_var_1)
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  24 happyReduction_21
happyReduction_21 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn24
		 (Eqs (mkPosToken happy_var_1)
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  25 happyReduction_22
happyReduction_22 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn25
		 (ConcatS (mkPosToken happy_var_1)
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  26 happyReduction_23
happyReduction_23 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (Add (mkPosToken happy_var_1)
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  27 happyReduction_24
happyReduction_24 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (Subtract (mkPosToken happy_var_1)
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  28 happyReduction_25
happyReduction_25 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn28
		 (Mul (mkPosToken happy_var_1)
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  29 happyReduction_26
happyReduction_26 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (Quot (mkPosToken happy_var_1)
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  30 happyReduction_27
happyReduction_27 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn30
		 (Rem (mkPosToken happy_var_1)
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  31 happyReduction_28
happyReduction_28 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn31
		 (Cons (mkPosToken happy_var_1)
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  32 happyReduction_29
happyReduction_29 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn32
		 (Case (mkPosToken happy_var_1)
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  33 happyReduction_30
happyReduction_30 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn33
		 (If (mkPosToken happy_var_1)
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  34 happyReduction_31
happyReduction_31 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn34
		 (Rec (mkPosToken happy_var_1)
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  35 happyReduction_32
happyReduction_32 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn35
		 (Get (mkPosToken happy_var_1)
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  36 happyReduction_33
happyReduction_33 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (Put (mkPosToken happy_var_1)
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  37 happyReduction_34
happyReduction_34 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn37
		 (Hput (mkPosToken happy_var_1)
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  38 happyReduction_35
happyReduction_35 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn38
		 (Hcase (mkPosToken happy_var_1)
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  39 happyReduction_36
happyReduction_36 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn39
		 (Split (mkPosToken happy_var_1)
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  40 happyReduction_37
happyReduction_37 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn40
		 (Fork (mkPosToken happy_var_1)
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  41 happyReduction_38
happyReduction_38 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn41
		 (Plug (mkPosToken happy_var_1)
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  42 happyReduction_39
happyReduction_39 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn42
		 (Run (mkPosToken happy_var_1)
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  43 happyReduction_40
happyReduction_40 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn43
		 (Race (mkPosToken happy_var_1)
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  44 happyReduction_41
happyReduction_41 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn44
		 (Close (mkPosToken happy_var_1)
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  45 happyReduction_42
happyReduction_42 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn45
		 (Halt (mkPosToken happy_var_1)
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  46 happyReduction_43
happyReduction_43 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn46
		 (Ch_Id (mkPosToken happy_var_1)
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  47 happyReduction_44
happyReduction_44 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn47
		 (Main_run (mkPosToken happy_var_1)
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  48 happyReduction_45
happyReduction_45 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn48
		 (BTrue (mkPosToken happy_var_1)
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  49 happyReduction_46
happyReduction_46 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (BFalse (mkPosToken happy_var_1)
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  50 happyReduction_47
happyReduction_47 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn50
		 (Character (mkPosToken happy_var_1)
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  51 happyReduction_48
happyReduction_48 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn51
		 (UIdent (mkPosToken happy_var_1)
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  52 happyReduction_49
happyReduction_49 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn52
		 (PIdent (mkPosToken happy_var_1)
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  53 happyReduction_50
happyReduction_50 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn53
		 (PInteger (mkPosToken happy_var_1)
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  54 happyReduction_51
happyReduction_51 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn54
		 (IIdent (mkPosToken happy_var_1)
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_2  55 happyReduction_52
happyReduction_52 (HappyAbsSyn79  happy_var_2)
	(HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn55
		 (Language.AbsAMPL.Main (reverse happy_var_1) happy_var_2
	)
happyReduction_52 _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  56 happyReduction_53
happyReduction_53 (HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn56
		 (Language.AbsAMPL.IMPORT_CONSTRUCT happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  56 happyReduction_54
happyReduction_54 (HappyAbsSyn69  happy_var_1)
	 =  HappyAbsSyn56
		 (Language.AbsAMPL.HANDLE_CONSTRUCT happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  56 happyReduction_55
happyReduction_55 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn56
		 (Language.AbsAMPL.COHANDLE_CONSTRUCT happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  56 happyReduction_56
happyReduction_56 (HappyAbsSyn63  happy_var_1)
	 =  HappyAbsSyn56
		 (Language.AbsAMPL.CONSTRUCTOR_CONSTRUCT happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  56 happyReduction_57
happyReduction_57 (HappyAbsSyn64  happy_var_1)
	 =  HappyAbsSyn56
		 (Language.AbsAMPL.DESTRUCTOR_CONSTRUCT happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  56 happyReduction_58
happyReduction_58 (HappyAbsSyn71  happy_var_1)
	 =  HappyAbsSyn56
		 (Language.AbsAMPL.PROCESSES_CONSTRUCT happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  56 happyReduction_59
happyReduction_59 (HappyAbsSyn76  happy_var_1)
	 =  HappyAbsSyn56
		 (Language.AbsAMPL.FUNCTIONS_CONSTRUCT happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_0  57 happyReduction_60
happyReduction_60  =  HappyAbsSyn57
		 ([]
	)

happyReduce_61 = happySpecReduce_2  57 happyReduction_61
happyReduction_61 (HappyAbsSyn56  happy_var_2)
	(HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn57
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happyReduce 5 58 happyReduction_62
happyReduction_62 (_ `HappyStk`
	(HappyAbsSyn61  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn58
		 (Language.AbsAMPL.Hand_spec happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_63 = happySpecReduce_1  59 happyReduction_63
happyReduction_63 (HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn59
		 (Language.AbsAMPL.HandName happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_0  60 happyReduction_64
happyReduction_64  =  HappyAbsSyn60
		 ([]
	)

happyReduce_65 = happySpecReduce_1  60 happyReduction_65
happyReduction_65 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn60
		 ((:[]) happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  60 happyReduction_66
happyReduction_66 (HappyAbsSyn60  happy_var_3)
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn60
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  61 happyReduction_67
happyReduction_67 (HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn61
		 ((:[]) happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  61 happyReduction_68
happyReduction_68 (HappyAbsSyn61  happy_var_3)
	_
	(HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn61
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_2  62 happyReduction_69
happyReduction_69 (HappyAbsSyn54  happy_var_2)
	_
	 =  HappyAbsSyn62
		 (Language.AbsAMPL.Import happy_var_2
	)
happyReduction_69 _ _  = notHappyAtAll 

happyReduce_70 = happyReduce 5 63 happyReduction_70
happyReduction_70 (_ `HappyStk`
	(HappyAbsSyn67  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn63
		 (Language.AbsAMPL.Constructors happy_var_4
	) `HappyStk` happyRest

happyReduce_71 = happyReduce 5 64 happyReduction_71
happyReduction_71 (_ `HappyStk`
	(HappyAbsSyn67  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn64
		 (Language.AbsAMPL.Destructors happy_var_4
	) `HappyStk` happyRest

happyReduce_72 = happyReduce 5 65 happyReduction_72
happyReduction_72 (_ `HappyStk`
	(HappyAbsSyn68  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn65
		 (Language.AbsAMPL.Struct_spec happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_73 = happySpecReduce_2  66 happyReduction_73
happyReduction_73 (HappyAbsSyn53  happy_var_2)
	(HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn66
		 (Language.AbsAMPL.Struct happy_var_1 happy_var_2
	)
happyReduction_73 _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_0  67 happyReduction_74
happyReduction_74  =  HappyAbsSyn67
		 ([]
	)

happyReduce_75 = happySpecReduce_1  67 happyReduction_75
happyReduction_75 (HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn67
		 ((:[]) happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_3  67 happyReduction_76
happyReduction_76 (HappyAbsSyn67  happy_var_3)
	_
	(HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn67
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_76 _ _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  68 happyReduction_77
happyReduction_77 (HappyAbsSyn66  happy_var_1)
	 =  HappyAbsSyn68
		 ((:[]) happy_var_1
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_3  68 happyReduction_78
happyReduction_78 (HappyAbsSyn68  happy_var_3)
	_
	(HappyAbsSyn66  happy_var_1)
	 =  HappyAbsSyn68
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happyReduce 5 69 happyReduction_79
happyReduction_79 (_ `HappyStk`
	(HappyAbsSyn60  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn69
		 (Language.AbsAMPL.Handles happy_var_4
	) `HappyStk` happyRest

happyReduce_80 = happyReduce 5 70 happyReduction_80
happyReduction_80 (_ `HappyStk`
	(HappyAbsSyn60  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn70
		 (Language.AbsAMPL.Cohandles happy_var_4
	) `HappyStk` happyRest

happyReduce_81 = happyReduce 5 71 happyReduction_81
happyReduction_81 (_ `HappyStk`
	(HappyAbsSyn72  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn71
		 (Language.AbsAMPL.Processes happy_var_4
	) `HappyStk` happyRest

happyReduce_82 = happySpecReduce_0  72 happyReduction_82
happyReduction_82  =  HappyAbsSyn72
		 ([]
	)

happyReduce_83 = happySpecReduce_1  72 happyReduction_83
happyReduction_83 (HappyAbsSyn73  happy_var_1)
	 =  HappyAbsSyn72
		 ((:[]) happy_var_1
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_3  72 happyReduction_84
happyReduction_84 (HappyAbsSyn72  happy_var_3)
	_
	(HappyAbsSyn73  happy_var_1)
	 =  HappyAbsSyn72
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happyReduce 10 73 happyReduction_85
happyReduction_85 ((HappyAbsSyn81  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn89  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn89  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn75  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn73
		 (Language.AbsAMPL.Process_spec happy_var_1 happy_var_3 happy_var_5 happy_var_7 happy_var_10
	) `HappyStk` happyRest

happyReduce_86 = happySpecReduce_1  74 happyReduction_86
happyReduction_86 (HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn74
		 (Language.AbsAMPL.VName happy_var_1
	)
happyReduction_86 _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_0  75 happyReduction_87
happyReduction_87  =  HappyAbsSyn75
		 ([]
	)

happyReduce_88 = happySpecReduce_1  75 happyReduction_88
happyReduction_88 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn75
		 ((:[]) happy_var_1
	)
happyReduction_88 _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_3  75 happyReduction_89
happyReduction_89 (HappyAbsSyn75  happy_var_3)
	_
	(HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn75
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_89 _ _ _  = notHappyAtAll 

happyReduce_90 = happyReduce 5 76 happyReduction_90
happyReduction_90 (_ `HappyStk`
	(HappyAbsSyn77  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn76
		 (Language.AbsAMPL.Functions happy_var_4
	) `HappyStk` happyRest

happyReduce_91 = happySpecReduce_0  77 happyReduction_91
happyReduction_91  =  HappyAbsSyn77
		 ([]
	)

happyReduce_92 = happySpecReduce_1  77 happyReduction_92
happyReduction_92 (HappyAbsSyn78  happy_var_1)
	 =  HappyAbsSyn77
		 ((:[]) happy_var_1
	)
happyReduction_92 _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_3  77 happyReduction_93
happyReduction_93 (HappyAbsSyn77  happy_var_3)
	_
	(HappyAbsSyn78  happy_var_1)
	 =  HappyAbsSyn77
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_93 _ _ _  = notHappyAtAll 

happyReduce_94 = happyReduce 6 78 happyReduction_94
happyReduction_94 ((HappyAbsSyn81  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn75  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn78
		 (Language.AbsAMPL.Function_spec happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_95 = happyReduce 4 79 happyReduction_95
happyReduction_95 ((HappyAbsSyn81  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn80  happy_var_2) `HappyStk`
	(HappyAbsSyn47  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn79
		 (Language.AbsAMPL.Start happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_96 = happySpecReduce_0  79 happyReduction_96
happyReduction_96  =  HappyAbsSyn79
		 (Language.AbsAMPL.Start_none
	)

happyReduce_97 = happyReduce 6 80 happyReduction_97
happyReduction_97 (_ `HappyStk`
	(HappyAbsSyn89  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn89  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn80
		 (Language.AbsAMPL.Channel_spec happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_98 = happySpecReduce_3  81 happyReduction_98
happyReduction_98 _
	(HappyAbsSyn82  happy_var_2)
	_
	 =  HappyAbsSyn81
		 (Language.AbsAMPL.Prog happy_var_2
	)
happyReduction_98 _ _ _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_0  82 happyReduction_99
happyReduction_99  =  HappyAbsSyn82
		 ([]
	)

happyReduce_100 = happySpecReduce_1  82 happyReduction_100
happyReduction_100 (HappyAbsSyn83  happy_var_1)
	 =  HappyAbsSyn82
		 ((:[]) happy_var_1
	)
happyReduction_100 _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_3  82 happyReduction_101
happyReduction_101 (HappyAbsSyn82  happy_var_3)
	_
	(HappyAbsSyn83  happy_var_1)
	 =  HappyAbsSyn82
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_101 _ _ _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_3  83 happyReduction_102
happyReduction_102 (HappyAbsSyn83  happy_var_3)
	_
	(HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_ASSIGN happy_var_1 happy_var_3
	)
happyReduction_102 _ _ _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_2  83 happyReduction_103
happyReduction_103 (HappyAbsSyn52  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_STOREf happy_var_1 happy_var_2
	)
happyReduction_103 _ _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_2  83 happyReduction_104
happyReduction_104 (HappyAbsSyn52  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_LOADf happy_var_1 happy_var_2
	)
happyReduction_104 _ _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_1  83 happyReduction_105
happyReduction_105 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_RET happy_var_1
	)
happyReduction_105 _  = notHappyAtAll 

happyReduce_106 = happyReduce 5 83 happyReduction_106
happyReduction_106 (_ `HappyStk`
	(HappyAbsSyn89  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_2) `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (Language.AbsAMPL.AC_CALLf happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_107 = happySpecReduce_2  83 happyReduction_107
happyReduction_107 (HappyAbsSyn90  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_INT happy_var_1 happy_var_2
	)
happyReduction_107 _ _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_2  83 happyReduction_108
happyReduction_108 (HappyAbsSyn50  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_CHAR happy_var_1 happy_var_2
	)
happyReduction_108 _ _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_2  83 happyReduction_109
happyReduction_109 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_STRING happy_var_1 happy_var_2
	)
happyReduction_109 _ _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_1  83 happyReduction_110
happyReduction_110 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_TOSTR happy_var_1
	)
happyReduction_110 _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_1  83 happyReduction_111
happyReduction_111 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_TOINT happy_var_1
	)
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_1  83 happyReduction_112
happyReduction_112 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_AND happy_var_1
	)
happyReduction_112 _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_1  83 happyReduction_113
happyReduction_113 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_OR happy_var_1
	)
happyReduction_113 _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_1  83 happyReduction_114
happyReduction_114 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_APPEND happy_var_1
	)
happyReduction_114 _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_1  83 happyReduction_115
happyReduction_115 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_TRUE happy_var_1
	)
happyReduction_115 _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_1  83 happyReduction_116
happyReduction_116 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_FALSE happy_var_1
	)
happyReduction_116 _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_1  83 happyReduction_117
happyReduction_117 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_UNSTRING happy_var_1
	)
happyReduction_117 _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_1  83 happyReduction_118
happyReduction_118 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_LEQ happy_var_1
	)
happyReduction_118 _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_1  83 happyReduction_119
happyReduction_119 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_EQ happy_var_1
	)
happyReduction_119 _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_1  83 happyReduction_120
happyReduction_120 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_LEQC happy_var_1
	)
happyReduction_120 _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_1  83 happyReduction_121
happyReduction_121 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_EQC happy_var_1
	)
happyReduction_121 _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_1  83 happyReduction_122
happyReduction_122 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_LEQS happy_var_1
	)
happyReduction_122 _  = notHappyAtAll 

happyReduce_123 = happySpecReduce_1  83 happyReduction_123
happyReduction_123 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_EQS happy_var_1
	)
happyReduction_123 _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_2  83 happyReduction_124
happyReduction_124 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_CONCAT happy_var_1 happy_var_2
	)
happyReduction_124 _ _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_1  83 happyReduction_125
happyReduction_125 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_ADD happy_var_1
	)
happyReduction_125 _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_1  83 happyReduction_126
happyReduction_126 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_SUB happy_var_1
	)
happyReduction_126 _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_1  83 happyReduction_127
happyReduction_127 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_MUL happy_var_1
	)
happyReduction_127 _  = notHappyAtAll 

happyReduce_128 = happySpecReduce_1  83 happyReduction_128
happyReduction_128 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_DIVQ happy_var_1
	)
happyReduction_128 _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_1  83 happyReduction_129
happyReduction_129 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_DIVR happy_var_1
	)
happyReduction_129 _  = notHappyAtAll 

happyReduce_130 = happyReduce 6 83 happyReduction_130
happyReduction_130 (_ `HappyStk`
	(HappyAbsSyn53  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn31  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (Language.AbsAMPL.AC_CONS happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_131 = happySpecReduce_3  83 happyReduction_131
happyReduction_131 (HappyAbsSyn51  happy_var_3)
	_
	(HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_STRUCT happy_var_1 happy_var_3
	)
happyReduction_131 _ _ _  = notHappyAtAll 

happyReduce_132 = happyReduce 6 83 happyReduction_132
happyReduction_132 (_ `HappyStk`
	(HappyAbsSyn89  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (Language.AbsAMPL.AC_STRUCTAS happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_133 = happyReduce 6 83 happyReduction_133
happyReduction_133 (_ `HappyStk`
	(HappyAbsSyn86  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_2) `HappyStk`
	(HappyAbsSyn32  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (Language.AbsAMPL.AC_CASEf happy_var_1 happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_134 = happyReduce 6 83 happyReduction_134
happyReduction_134 ((HappyAbsSyn81  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn81  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_2) `HappyStk`
	(HappyAbsSyn33  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (Language.AbsAMPL.AC_IF happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_135 = happyReduce 5 83 happyReduction_135
happyReduction_135 (_ `HappyStk`
	(HappyAbsSyn86  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (Language.AbsAMPL.AC_RECORDf happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_136 = happyReduce 4 83 happyReduction_136
happyReduction_136 ((HappyAbsSyn52  happy_var_4) `HappyStk`
	(HappyAbsSyn51  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (Language.AbsAMPL.AC_DEST happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_137 = happyReduce 7 83 happyReduction_137
happyReduction_137 ((HappyAbsSyn52  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn89  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (Language.AbsAMPL.AC_DESTAS happy_var_1 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_138 = happyReduce 4 83 happyReduction_138
happyReduction_138 ((HappyAbsSyn52  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_2) `HappyStk`
	(HappyAbsSyn35  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (Language.AbsAMPL.AC_GETf happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_139 = happyReduce 6 83 happyReduction_139
happyReduction_139 ((HappyAbsSyn52  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_2) `HappyStk`
	(HappyAbsSyn37  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (Language.AbsAMPL.AC_HPUTf happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_140 = happyReduce 6 83 happyReduction_140
happyReduction_140 (_ `HappyStk`
	(HappyAbsSyn86  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_2) `HappyStk`
	(HappyAbsSyn38  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (Language.AbsAMPL.AC_HCASEf happy_var_1 happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_141 = happyReduce 4 83 happyReduction_141
happyReduction_141 ((HappyAbsSyn52  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_2) `HappyStk`
	(HappyAbsSyn36  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (Language.AbsAMPL.AC_PUTf happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_142 = happyReduce 5 83 happyReduction_142
happyReduction_142 ((HappyAbsSyn52  happy_var_5) `HappyStk`
	(HappyAbsSyn52  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_2) `HappyStk`
	(HappyAbsSyn39  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (Language.AbsAMPL.AC_SPLITf happy_var_1 happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_143 = happyReduce 16 83 happyReduction_143
happyReduction_143 (_ `HappyStk`
	(HappyAbsSyn81  happy_var_15) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn89  happy_var_13) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_11) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn81  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn89  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_2) `HappyStk`
	(HappyAbsSyn40  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (Language.AbsAMPL.AC_FORKf happy_var_1 happy_var_2 happy_var_5 happy_var_7 happy_var_9 happy_var_11 happy_var_13 happy_var_15
	) `HappyStk` happyRest

happyReduce_144 = happyReduce 18 83 happyReduction_144
happyReduction_144 (_ `HappyStk`
	(HappyAbsSyn81  happy_var_17) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn89  happy_var_14) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn81  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn89  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn89  happy_var_2) `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (Language.AbsAMPL.AC_PLUGf happy_var_1 happy_var_2 happy_var_7 happy_var_10 happy_var_14 happy_var_17
	) `HappyStk` happyRest

happyReduce_145 = happyReduce 9 83 happyReduction_145
happyReduction_145 (_ `HappyStk`
	(HappyAbsSyn89  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn89  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn89  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_2) `HappyStk`
	(HappyAbsSyn42  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (Language.AbsAMPL.AC_RUNf happy_var_1 happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_146 = happySpecReduce_3  83 happyReduction_146
happyReduction_146 (HappyAbsSyn52  happy_var_3)
	(HappyAbsSyn46  happy_var_2)
	(HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_IDF happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_146 _ _ _  = notHappyAtAll 

happyReduce_147 = happyReduce 4 83 happyReduction_147
happyReduction_147 (_ `HappyStk`
	(HappyAbsSyn88  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn43  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (Language.AbsAMPL.AC_RACE happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_148 = happySpecReduce_3  83 happyReduction_148
happyReduction_148 _
	(HappyAbsSyn89  happy_var_2)
	_
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_PROD happy_var_2
	)
happyReduction_148 _ _ _  = notHappyAtAll 

happyReduce_149 = happyReduce 5 83 happyReduction_149
happyReduction_149 (_ `HappyStk`
	(HappyAbsSyn52  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (Language.AbsAMPL.AC_PRODELEM happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_150 = happySpecReduce_1  83 happyReduction_150
happyReduction_150 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_EMSG happy_var_1
	)
happyReduction_150 _  = notHappyAtAll 

happyReduce_151 = happySpecReduce_2  83 happyReduction_151
happyReduction_151 (HappyAbsSyn52  happy_var_2)
	(HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_CLOSEf happy_var_1 happy_var_2
	)
happyReduction_151 _ _  = notHappyAtAll 

happyReduce_152 = happySpecReduce_2  83 happyReduction_152
happyReduction_152 (HappyAbsSyn52  happy_var_2)
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn83
		 (Language.AbsAMPL.AC_HALTf happy_var_1 happy_var_2
	)
happyReduction_152 _ _  = notHappyAtAll 

happyReduce_153 = happyReduce 5 84 happyReduction_153
happyReduction_153 ((HappyAbsSyn81  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn84
		 (Language.AbsAMPL.Labelcoms1 happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_154 = happyReduce 8 84 happyReduction_154
happyReduction_154 ((HappyAbsSyn81  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn89  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn84
		 (Language.AbsAMPL.Labelcoms2 happy_var_1 happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_155 = happySpecReduce_0  85 happyReduction_155
happyReduction_155  =  HappyAbsSyn85
		 ([]
	)

happyReduce_156 = happySpecReduce_1  85 happyReduction_156
happyReduction_156 (HappyAbsSyn81  happy_var_1)
	 =  HappyAbsSyn85
		 ((:[]) happy_var_1
	)
happyReduction_156 _  = notHappyAtAll 

happyReduce_157 = happySpecReduce_3  85 happyReduction_157
happyReduction_157 (HappyAbsSyn85  happy_var_3)
	_
	(HappyAbsSyn81  happy_var_1)
	 =  HappyAbsSyn85
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_157 _ _ _  = notHappyAtAll 

happyReduce_158 = happySpecReduce_0  86 happyReduction_158
happyReduction_158  =  HappyAbsSyn86
		 ([]
	)

happyReduce_159 = happySpecReduce_1  86 happyReduction_159
happyReduction_159 (HappyAbsSyn84  happy_var_1)
	 =  HappyAbsSyn86
		 ((:[]) happy_var_1
	)
happyReduction_159 _  = notHappyAtAll 

happyReduce_160 = happySpecReduce_3  86 happyReduction_160
happyReduction_160 (HappyAbsSyn86  happy_var_3)
	_
	(HappyAbsSyn84  happy_var_1)
	 =  HappyAbsSyn86
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_160 _ _ _  = notHappyAtAll 

happyReduce_161 = happySpecReduce_3  87 happyReduction_161
happyReduction_161 (HappyAbsSyn81  happy_var_3)
	_
	(HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn87
		 (Language.AbsAMPL.Races happy_var_1 happy_var_3
	)
happyReduction_161 _ _ _  = notHappyAtAll 

happyReduce_162 = happySpecReduce_0  88 happyReduction_162
happyReduction_162  =  HappyAbsSyn88
		 ([]
	)

happyReduce_163 = happySpecReduce_1  88 happyReduction_163
happyReduction_163 (HappyAbsSyn87  happy_var_1)
	 =  HappyAbsSyn88
		 ((:[]) happy_var_1
	)
happyReduction_163 _  = notHappyAtAll 

happyReduce_164 = happySpecReduce_3  88 happyReduction_164
happyReduction_164 (HappyAbsSyn88  happy_var_3)
	_
	(HappyAbsSyn87  happy_var_1)
	 =  HappyAbsSyn88
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_164 _ _ _  = notHappyAtAll 

happyReduce_165 = happySpecReduce_0  89 happyReduction_165
happyReduction_165  =  HappyAbsSyn89
		 ([]
	)

happyReduce_166 = happySpecReduce_1  89 happyReduction_166
happyReduction_166 (HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn89
		 ((:[]) happy_var_1
	)
happyReduction_166 _  = notHappyAtAll 

happyReduce_167 = happySpecReduce_3  89 happyReduction_167
happyReduction_167 (HappyAbsSyn89  happy_var_3)
	_
	(HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn89
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_167 _ _ _  = notHappyAtAll 

happyReduce_168 = happySpecReduce_1  90 happyReduction_168
happyReduction_168 (HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn90
		 (Language.AbsAMPL.Positive happy_var_1
	)
happyReduction_168 _  = notHappyAtAll 

happyReduce_169 = happySpecReduce_2  90 happyReduction_169
happyReduction_169 (HappyAbsSyn53  happy_var_2)
	_
	 =  HappyAbsSyn90
		 (Language.AbsAMPL.Negative happy_var_2
	)
happyReduction_169 _ _  = notHappyAtAll 

happyReduce_170 = happySpecReduce_0  91 happyReduction_170
happyReduction_170  =  HappyAbsSyn91
		 ([]
	)

happyReduce_171 = happySpecReduce_1  91 happyReduction_171
happyReduction_171 (HappyAbsSyn90  happy_var_1)
	 =  HappyAbsSyn91
		 ((:[]) happy_var_1
	)
happyReduction_171 _  = notHappyAtAll 

happyReduce_172 = happySpecReduce_3  91 happyReduction_172
happyReduction_172 (HappyAbsSyn91  happy_var_3)
	_
	(HappyAbsSyn90  happy_var_1)
	 =  HappyAbsSyn91
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_172 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 174 174 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 92;
	PT _ (TS _ 2) -> cont 93;
	PT _ (TS _ 3) -> cont 94;
	PT _ (TS _ 4) -> cont 95;
	PT _ (TS _ 5) -> cont 96;
	PT _ (TS _ 6) -> cont 97;
	PT _ (TS _ 7) -> cont 98;
	PT _ (TS _ 8) -> cont 99;
	PT _ (TS _ 9) -> cont 100;
	PT _ (TS _ 10) -> cont 101;
	PT _ (TS _ 11) -> cont 102;
	PT _ (TS _ 12) -> cont 103;
	PT _ (TS _ 13) -> cont 104;
	PT _ (TS _ 14) -> cont 105;
	PT _ (TS _ 15) -> cont 106;
	PT _ (TS _ 16) -> cont 107;
	PT _ (TS _ 17) -> cont 108;
	PT _ (TS _ 18) -> cont 109;
	PT _ (TS _ 19) -> cont 110;
	PT _ (TS _ 20) -> cont 111;
	PT _ (TS _ 21) -> cont 112;
	PT _ (TS _ 22) -> cont 113;
	PT _ (TS _ 23) -> cont 114;
	PT _ (TS _ 24) -> cont 115;
	PT _ (TS _ 25) -> cont 116;
	PT _ (TS _ 26) -> cont 117;
	PT _ (TS _ 27) -> cont 118;
	PT _ (TS _ 28) -> cont 119;
	PT _ (TS _ 29) -> cont 120;
	PT _ (TS _ 30) -> cont 121;
	PT _ (TS _ 31) -> cont 122;
	PT _ (TL happy_dollar_dollar) -> cont 123;
	PT _ (TI happy_dollar_dollar) -> cont 124;
	PT _ (T_Store _) -> cont 125;
	PT _ (T_Load _) -> cont 126;
	PT _ (T_Ret _) -> cont 127;
	PT _ (T_Call _) -> cont 128;
	PT _ (T_ConstInt _) -> cont 129;
	PT _ (T_ConstChar _) -> cont 130;
	PT _ (T_ConstString _) -> cont 131;
	PT _ (T_ToStr _) -> cont 132;
	PT _ (T_ToInt _) -> cont 133;
	PT _ (T_And _) -> cont 134;
	PT _ (T_Or _) -> cont 135;
	PT _ (T_Append _) -> cont 136;
	PT _ (T_Unstring _) -> cont 137;
	PT _ (T_LeqI _) -> cont 138;
	PT _ (T_EqI _) -> cont 139;
	PT _ (T_Leqc _) -> cont 140;
	PT _ (T_Eqc _) -> cont 141;
	PT _ (T_Leqs _) -> cont 142;
	PT _ (T_Eqs _) -> cont 143;
	PT _ (T_ConcatS _) -> cont 144;
	PT _ (T_Add _) -> cont 145;
	PT _ (T_Subtract _) -> cont 146;
	PT _ (T_Mul _) -> cont 147;
	PT _ (T_Quot _) -> cont 148;
	PT _ (T_Rem _) -> cont 149;
	PT _ (T_Cons _) -> cont 150;
	PT _ (T_Case _) -> cont 151;
	PT _ (T_If _) -> cont 152;
	PT _ (T_Rec _) -> cont 153;
	PT _ (T_Get _) -> cont 154;
	PT _ (T_Put _) -> cont 155;
	PT _ (T_Hput _) -> cont 156;
	PT _ (T_Hcase _) -> cont 157;
	PT _ (T_Split _) -> cont 158;
	PT _ (T_Fork _) -> cont 159;
	PT _ (T_Plug _) -> cont 160;
	PT _ (T_Run _) -> cont 161;
	PT _ (T_Race _) -> cont 162;
	PT _ (T_Close _) -> cont 163;
	PT _ (T_Halt _) -> cont 164;
	PT _ (T_Ch_Id _) -> cont 165;
	PT _ (T_Main_run _) -> cont 166;
	PT _ (T_BTrue _) -> cont 167;
	PT _ (T_BFalse _) -> cont 168;
	PT _ (T_Character _) -> cont 169;
	PT _ (T_UIdent _) -> cont 170;
	PT _ (T_PIdent _) -> cont 171;
	PT _ (T_PInteger _) -> cont 172;
	PT _ (T_IIdent _) -> cont 173;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 174 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pAMPLCODE tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn55 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
