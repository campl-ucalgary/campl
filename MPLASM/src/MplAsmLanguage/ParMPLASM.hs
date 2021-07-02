{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module MplAsmLanguage.ParMPLASM
  ( happyError
  , myLexer
  , pAMPLCODE
  ) where
import qualified MplAsmLanguage.AbsMPLASM
import MplAsmLanguage.LexMPLASM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (Integer)
	| HappyAbsSyn5 (String)
	| HappyAbsSyn6 (MplAsmLanguage.AbsMPLASM.Store)
	| HappyAbsSyn7 (MplAsmLanguage.AbsMPLASM.Load)
	| HappyAbsSyn8 (MplAsmLanguage.AbsMPLASM.Ret)
	| HappyAbsSyn9 (MplAsmLanguage.AbsMPLASM.Call)
	| HappyAbsSyn10 (MplAsmLanguage.AbsMPLASM.CInt)
	| HappyAbsSyn11 (MplAsmLanguage.AbsMPLASM.CChar)
	| HappyAbsSyn12 (MplAsmLanguage.AbsMPLASM.CBool)
	| HappyAbsSyn13 (MplAsmLanguage.AbsMPLASM.CString)
	| HappyAbsSyn14 (MplAsmLanguage.AbsMPLASM.ToStr)
	| HappyAbsSyn15 (MplAsmLanguage.AbsMPLASM.ToInt)
	| HappyAbsSyn16 (MplAsmLanguage.AbsMPLASM.And)
	| HappyAbsSyn17 (MplAsmLanguage.AbsMPLASM.Or)
	| HappyAbsSyn18 (MplAsmLanguage.AbsMPLASM.Append)
	| HappyAbsSyn19 (MplAsmLanguage.AbsMPLASM.Unstring)
	| HappyAbsSyn20 (MplAsmLanguage.AbsMPLASM.LeqI)
	| HappyAbsSyn21 (MplAsmLanguage.AbsMPLASM.EqI)
	| HappyAbsSyn22 (MplAsmLanguage.AbsMPLASM.EqB)
	| HappyAbsSyn23 (MplAsmLanguage.AbsMPLASM.LeqC)
	| HappyAbsSyn24 (MplAsmLanguage.AbsMPLASM.EqC)
	| HappyAbsSyn25 (MplAsmLanguage.AbsMPLASM.Leqs)
	| HappyAbsSyn26 (MplAsmLanguage.AbsMPLASM.Eqs)
	| HappyAbsSyn27 (MplAsmLanguage.AbsMPLASM.ConcatS)
	| HappyAbsSyn28 (MplAsmLanguage.AbsMPLASM.Add)
	| HappyAbsSyn29 (MplAsmLanguage.AbsMPLASM.Subtract)
	| HappyAbsSyn30 (MplAsmLanguage.AbsMPLASM.Mul)
	| HappyAbsSyn31 (MplAsmLanguage.AbsMPLASM.Quot)
	| HappyAbsSyn32 (MplAsmLanguage.AbsMPLASM.Rem)
	| HappyAbsSyn33 (MplAsmLanguage.AbsMPLASM.Case)
	| HappyAbsSyn34 (MplAsmLanguage.AbsMPLASM.If)
	| HappyAbsSyn35 (MplAsmLanguage.AbsMPLASM.Rec)
	| HappyAbsSyn36 (MplAsmLanguage.AbsMPLASM.Get)
	| HappyAbsSyn37 (MplAsmLanguage.AbsMPLASM.Put)
	| HappyAbsSyn38 (MplAsmLanguage.AbsMPLASM.Hput)
	| HappyAbsSyn39 (MplAsmLanguage.AbsMPLASM.Shput)
	| HappyAbsSyn40 (MplAsmLanguage.AbsMPLASM.Hcase)
	| HappyAbsSyn41 (MplAsmLanguage.AbsMPLASM.Split)
	| HappyAbsSyn42 (MplAsmLanguage.AbsMPLASM.Fork)
	| HappyAbsSyn43 (MplAsmLanguage.AbsMPLASM.Plug)
	| HappyAbsSyn44 (MplAsmLanguage.AbsMPLASM.Run)
	| HappyAbsSyn45 (MplAsmLanguage.AbsMPLASM.Race)
	| HappyAbsSyn46 (MplAsmLanguage.AbsMPLASM.Close)
	| HappyAbsSyn47 (MplAsmLanguage.AbsMPLASM.Halt)
	| HappyAbsSyn48 (MplAsmLanguage.AbsMPLASM.Ch_Id)
	| HappyAbsSyn49 (MplAsmLanguage.AbsMPLASM.Main_run)
	| HappyAbsSyn50 (MplAsmLanguage.AbsMPLASM.BBool)
	| HappyAbsSyn51 (MplAsmLanguage.AbsMPLASM.Character)
	| HappyAbsSyn52 (MplAsmLanguage.AbsMPLASM.UIdent)
	| HappyAbsSyn53 (MplAsmLanguage.AbsMPLASM.PIdent)
	| HappyAbsSyn54 (MplAsmLanguage.AbsMPLASM.PInteger)
	| HappyAbsSyn55 (MplAsmLanguage.AbsMPLASM.IIdent)
	| HappyAbsSyn56 (MplAsmLanguage.AbsMPLASM.AMPLCODE)
	| HappyAbsSyn57 (MplAsmLanguage.AbsMPLASM.AmplConstructs)
	| HappyAbsSyn58 ([MplAsmLanguage.AbsMPLASM.AmplConstructs])
	| HappyAbsSyn59 (MplAsmLanguage.AbsMPLASM.ProtocolCoprotocolSpec)
	| HappyAbsSyn60 (MplAsmLanguage.AbsMPLASM.Handle)
	| HappyAbsSyn61 ([MplAsmLanguage.AbsMPLASM.ProtocolCoprotocolSpec])
	| HappyAbsSyn62 ([MplAsmLanguage.AbsMPLASM.Handle])
	| HappyAbsSyn63 (MplAsmLanguage.AbsMPLASM.Import)
	| HappyAbsSyn64 (MplAsmLanguage.AbsMPLASM.Constructors)
	| HappyAbsSyn65 (MplAsmLanguage.AbsMPLASM.Destructors)
	| HappyAbsSyn66 (MplAsmLanguage.AbsMPLASM.StructorSpec)
	| HappyAbsSyn67 (MplAsmLanguage.AbsMPLASM.Struct)
	| HappyAbsSyn68 ([MplAsmLanguage.AbsMPLASM.StructorSpec])
	| HappyAbsSyn69 ([MplAsmLanguage.AbsMPLASM.Struct])
	| HappyAbsSyn70 (MplAsmLanguage.AbsMPLASM.Protocols)
	| HappyAbsSyn71 (MplAsmLanguage.AbsMPLASM.Coprotocols)
	| HappyAbsSyn72 (MplAsmLanguage.AbsMPLASM.Processes)
	| HappyAbsSyn73 ([MplAsmLanguage.AbsMPLASM.ProcessesSpec])
	| HappyAbsSyn74 (MplAsmLanguage.AbsMPLASM.ProcessesSpec)
	| HappyAbsSyn75 ([MplAsmLanguage.AbsMPLASM.PIdent])
	| HappyAbsSyn76 (MplAsmLanguage.AbsMPLASM.Functions)
	| HappyAbsSyn77 ([MplAsmLanguage.AbsMPLASM.FunctionsSpec])
	| HappyAbsSyn78 (MplAsmLanguage.AbsMPLASM.FunctionsSpec)
	| HappyAbsSyn79 (MplAsmLanguage.AbsMPLASM.Main)
	| HappyAbsSyn80 (MplAsmLanguage.AbsMPLASM.MainChannels)
	| HappyAbsSyn81 (MplAsmLanguage.AbsMPLASM.Coms)
	| HappyAbsSyn82 ([MplAsmLanguage.AbsMPLASM.Com])
	| HappyAbsSyn83 (MplAsmLanguage.AbsMPLASM.Com)
	| HappyAbsSyn84 (MplAsmLanguage.AbsMPLASM.LabelledComs)
	| HappyAbsSyn85 ([MplAsmLanguage.AbsMPLASM.Coms])
	| HappyAbsSyn86 ([MplAsmLanguage.AbsMPLASM.LabelledComs])
	| HappyAbsSyn87 (MplAsmLanguage.AbsMPLASM.RACE_PHRASE)
	| HappyAbsSyn88 ([MplAsmLanguage.AbsMPLASM.RACE_PHRASE])

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
 action_339 :: () => Prelude.Int -> ({-HappyReduction (Either String) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Either String) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Either String) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Either String) HappyAbsSyn)

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
 happyReduce_164 :: () => ({-HappyReduction (Either String) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Either String) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Either String) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Either String) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,590) ([0,0,0,0,0,65024,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4064,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,514,0,65535,65535,34815,1,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,256,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,128,49152,65535,65535,25087,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,257,32768,65535,65535,50175,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,4,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,528,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pAMPLCODE","Integer","String","Store","Load","Ret","Call","CInt","CChar","CBool","CString","ToStr","ToInt","And","Or","Append","Unstring","LeqI","EqI","EqB","LeqC","EqC","Leqs","Eqs","ConcatS","Add","Subtract","Mul","Quot","Rem","Case","If","Rec","Get","Put","Hput","Shput","Hcase","Split","Fork","Plug","Run","Race","Close","Halt","Ch_Id","Main_run","BBool","Character","UIdent","PIdent","PInteger","IIdent","AMPLCODE","AmplConstructs","ListAmplConstructs","ProtocolCoprotocolSpec","Handle","ListProtocolCoprotocolSpec","ListHandle","Import","Constructors","Destructors","StructorSpec","Struct","ListStructorSpec","ListStruct","Protocols","Coprotocols","Processes","ListProcessesSpec","ProcessesSpec","ListPIdent","Functions","ListFunctionsSpec","FunctionsSpec","Main","MainChannels","Coms","ListCom","Com","LabelledComs","ListComs","ListLabelledComs","RACE_PHRASE","ListRACE_PHRASE","'#'","'%constructors'","'%coprotocols'","'%destructors'","'%functions'","'%include'","'%processes'","'%protocols'","'('","')'","','","'->'","'.'","':'","':='","';'","'='","'=>'","'['","']'","'as'","'else'","'into'","'of'","'on'","'then'","'with'","'{'","'|'","'}'","L_integ","L_quoted","L_Store","L_Load","L_Ret","L_Call","L_CInt","L_CChar","L_CBool","L_CString","L_ToStr","L_ToInt","L_And","L_Or","L_Append","L_Unstring","L_LeqI","L_EqI","L_EqB","L_LeqC","L_EqC","L_Leqs","L_Eqs","L_ConcatS","L_Add","L_Subtract","L_Mul","L_Quot","L_Rem","L_Case","L_If","L_Rec","L_Get","L_Put","L_Hput","L_Shput","L_Hcase","L_Split","L_Fork","L_Plug","L_Run","L_Race","L_Close","L_Halt","L_Ch_Id","L_Main_run","L_BBool","L_Character","L_UIdent","L_PIdent","L_PInteger","L_IIdent","%eof"]
        bit_start = st Prelude.* 171
        bit_end = (st Prelude.+ 1) Prelude.* 171
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..170]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (90) = happyShift action_13
action_0 (91) = happyShift action_14
action_0 (92) = happyShift action_15
action_0 (93) = happyShift action_16
action_0 (94) = happyShift action_17
action_0 (95) = happyShift action_18
action_0 (96) = happyShift action_19
action_0 (56) = happyGoto action_3
action_0 (57) = happyGoto action_4
action_0 (58) = happyGoto action_5
action_0 (63) = happyGoto action_6
action_0 (64) = happyGoto action_7
action_0 (65) = happyGoto action_8
action_0 (70) = happyGoto action_9
action_0 (71) = happyGoto action_10
action_0 (72) = happyGoto action_11
action_0 (76) = happyGoto action_12
action_0 _ = happyReduce_61

action_1 (119) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (171) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (90) = happyShift action_13
action_4 (91) = happyShift action_14
action_4 (92) = happyShift action_15
action_4 (93) = happyShift action_16
action_4 (94) = happyShift action_17
action_4 (95) = happyShift action_18
action_4 (96) = happyShift action_19
action_4 (57) = happyGoto action_4
action_4 (58) = happyGoto action_31
action_4 (63) = happyGoto action_6
action_4 (64) = happyGoto action_7
action_4 (65) = happyGoto action_8
action_4 (70) = happyGoto action_9
action_4 (71) = happyGoto action_10
action_4 (72) = happyGoto action_11
action_4 (76) = happyGoto action_12
action_4 _ = happyReduce_61

action_5 (164) = happyShift action_30
action_5 (49) = happyGoto action_28
action_5 (79) = happyGoto action_29
action_5 _ = happyReduce_96

action_6 _ = happyReduce_54

action_7 _ = happyReduce_57

action_8 _ = happyReduce_58

action_9 _ = happyReduce_55

action_10 _ = happyReduce_56

action_11 _ = happyReduce_59

action_12 _ = happyReduce_60

action_13 (102) = happyShift action_27
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (102) = happyShift action_26
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (102) = happyShift action_25
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (102) = happyShift action_24
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (170) = happyShift action_23
action_17 (55) = happyGoto action_22
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (102) = happyShift action_21
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (102) = happyShift action_20
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (116) = happyShift action_39
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (116) = happyShift action_38
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_70

action_23 _ = happyReduce_52

action_24 (116) = happyShift action_37
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (116) = happyShift action_36
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (116) = happyShift action_35
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (116) = happyShift action_34
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (97) = happyShift action_33
action_28 (80) = happyGoto action_32
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_53

action_30 _ = happyReduce_46

action_31 _ = happyReduce_62

action_32 (102) = happyShift action_57
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (117) = happyShift action_56
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (167) = happyShift action_43
action_34 (52) = happyGoto action_51
action_34 (66) = happyGoto action_52
action_34 (68) = happyGoto action_55
action_34 _ = happyReduce_75

action_35 (167) = happyShift action_43
action_35 (52) = happyGoto action_40
action_35 (59) = happyGoto action_41
action_35 (61) = happyGoto action_54
action_35 _ = happyReduce_65

action_36 (167) = happyShift action_43
action_36 (52) = happyGoto action_51
action_36 (66) = happyGoto action_52
action_36 (68) = happyGoto action_53
action_36 _ = happyReduce_75

action_37 (168) = happyShift action_47
action_37 (53) = happyGoto action_48
action_37 (77) = happyGoto action_49
action_37 (78) = happyGoto action_50
action_37 _ = happyReduce_91

action_38 (168) = happyShift action_47
action_38 (53) = happyGoto action_44
action_38 (73) = happyGoto action_45
action_38 (74) = happyGoto action_46
action_38 _ = happyReduce_83

action_39 (167) = happyShift action_43
action_39 (52) = happyGoto action_40
action_39 (59) = happyGoto action_41
action_39 (61) = happyGoto action_42
action_39 _ = happyReduce_65

action_40 (105) = happyShift action_75
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (104) = happyShift action_74
action_41 _ = happyReduce_66

action_42 (118) = happyShift action_73
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_49

action_44 (97) = happyShift action_72
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (118) = happyShift action_71
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (104) = happyShift action_70
action_46 _ = happyReduce_84

action_47 _ = happyReduce_50

action_48 (97) = happyShift action_69
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (118) = happyShift action_68
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (104) = happyShift action_67
action_50 _ = happyReduce_92

action_51 (105) = happyShift action_66
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (104) = happyShift action_65
action_52 _ = happyReduce_76

action_53 (118) = happyShift action_64
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (118) = happyShift action_63
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (118) = happyShift action_62
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (168) = happyShift action_47
action_56 (53) = happyGoto action_60
action_56 (75) = happyGoto action_61
action_56 _ = happyReduce_87

action_57 (116) = happyShift action_59
action_57 (81) = happyGoto action_58
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_95

action_59 (89) = happyShift action_133
action_59 (97) = happyShift action_134
action_59 (120) = happyShift action_135
action_59 (121) = happyShift action_136
action_59 (122) = happyShift action_137
action_59 (123) = happyShift action_138
action_59 (124) = happyShift action_139
action_59 (125) = happyShift action_140
action_59 (126) = happyShift action_141
action_59 (127) = happyShift action_142
action_59 (128) = happyShift action_143
action_59 (129) = happyShift action_144
action_59 (130) = happyShift action_145
action_59 (131) = happyShift action_146
action_59 (132) = happyShift action_147
action_59 (133) = happyShift action_148
action_59 (134) = happyShift action_149
action_59 (135) = happyShift action_150
action_59 (136) = happyShift action_151
action_59 (137) = happyShift action_152
action_59 (138) = happyShift action_153
action_59 (139) = happyShift action_154
action_59 (140) = happyShift action_155
action_59 (141) = happyShift action_156
action_59 (142) = happyShift action_157
action_59 (143) = happyShift action_158
action_59 (144) = happyShift action_159
action_59 (145) = happyShift action_160
action_59 (146) = happyShift action_161
action_59 (147) = happyShift action_162
action_59 (148) = happyShift action_163
action_59 (149) = happyShift action_164
action_59 (150) = happyShift action_165
action_59 (151) = happyShift action_166
action_59 (152) = happyShift action_167
action_59 (153) = happyShift action_168
action_59 (154) = happyShift action_169
action_59 (155) = happyShift action_170
action_59 (156) = happyShift action_171
action_59 (157) = happyShift action_172
action_59 (158) = happyShift action_173
action_59 (159) = happyShift action_174
action_59 (160) = happyShift action_175
action_59 (161) = happyShift action_176
action_59 (162) = happyShift action_177
action_59 (167) = happyShift action_43
action_59 (168) = happyShift action_47
action_59 (5) = happyGoto action_86
action_59 (6) = happyGoto action_87
action_59 (7) = happyGoto action_88
action_59 (8) = happyGoto action_89
action_59 (9) = happyGoto action_90
action_59 (10) = happyGoto action_91
action_59 (11) = happyGoto action_92
action_59 (12) = happyGoto action_93
action_59 (13) = happyGoto action_94
action_59 (14) = happyGoto action_95
action_59 (15) = happyGoto action_96
action_59 (16) = happyGoto action_97
action_59 (17) = happyGoto action_98
action_59 (18) = happyGoto action_99
action_59 (19) = happyGoto action_100
action_59 (20) = happyGoto action_101
action_59 (21) = happyGoto action_102
action_59 (22) = happyGoto action_103
action_59 (23) = happyGoto action_104
action_59 (24) = happyGoto action_105
action_59 (25) = happyGoto action_106
action_59 (26) = happyGoto action_107
action_59 (27) = happyGoto action_108
action_59 (28) = happyGoto action_109
action_59 (29) = happyGoto action_110
action_59 (30) = happyGoto action_111
action_59 (31) = happyGoto action_112
action_59 (32) = happyGoto action_113
action_59 (33) = happyGoto action_114
action_59 (34) = happyGoto action_115
action_59 (35) = happyGoto action_116
action_59 (36) = happyGoto action_117
action_59 (37) = happyGoto action_118
action_59 (38) = happyGoto action_119
action_59 (39) = happyGoto action_120
action_59 (40) = happyGoto action_121
action_59 (41) = happyGoto action_122
action_59 (42) = happyGoto action_123
action_59 (43) = happyGoto action_124
action_59 (44) = happyGoto action_125
action_59 (45) = happyGoto action_126
action_59 (46) = happyGoto action_127
action_59 (47) = happyGoto action_128
action_59 (52) = happyGoto action_129
action_59 (53) = happyGoto action_130
action_59 (82) = happyGoto action_131
action_59 (83) = happyGoto action_132
action_59 _ = happyReduce_99

action_60 (99) = happyShift action_85
action_60 _ = happyReduce_88

action_61 (106) = happyShift action_84
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_71

action_63 _ = happyReduce_81

action_64 _ = happyReduce_72

action_65 (167) = happyShift action_43
action_65 (52) = happyGoto action_51
action_65 (66) = happyGoto action_52
action_65 (68) = happyGoto action_83
action_65 _ = happyReduce_75

action_66 (116) = happyShift action_82
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (168) = happyShift action_47
action_67 (53) = happyGoto action_48
action_67 (77) = happyGoto action_81
action_67 (78) = happyGoto action_50
action_67 _ = happyReduce_91

action_68 _ = happyReduce_90

action_69 (168) = happyShift action_47
action_69 (53) = happyGoto action_60
action_69 (75) = happyGoto action_80
action_69 _ = happyReduce_87

action_70 (168) = happyShift action_47
action_70 (53) = happyGoto action_44
action_70 (73) = happyGoto action_79
action_70 (74) = happyGoto action_46
action_70 _ = happyReduce_83

action_71 _ = happyReduce_82

action_72 (168) = happyShift action_47
action_72 (53) = happyGoto action_60
action_72 (75) = happyGoto action_78
action_72 _ = happyReduce_87

action_73 _ = happyReduce_80

action_74 (167) = happyShift action_43
action_74 (52) = happyGoto action_40
action_74 (59) = happyGoto action_41
action_74 (61) = happyGoto action_77
action_74 _ = happyReduce_65

action_75 (116) = happyShift action_76
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (167) = happyShift action_43
action_76 (52) = happyGoto action_219
action_76 (60) = happyGoto action_220
action_76 (62) = happyGoto action_221
action_76 _ = happyFail (happyExpListPerState 76)

action_77 _ = happyReduce_67

action_78 (117) = happyShift action_218
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_85

action_80 (98) = happyShift action_217
action_80 _ = happyFail (happyExpListPerState 80)

action_81 _ = happyReduce_93

action_82 (167) = happyShift action_43
action_82 (52) = happyGoto action_214
action_82 (67) = happyGoto action_215
action_82 (69) = happyGoto action_216
action_82 _ = happyFail (happyExpListPerState 82)

action_83 _ = happyReduce_77

action_84 (168) = happyShift action_47
action_84 (53) = happyGoto action_60
action_84 (75) = happyGoto action_213
action_84 _ = happyReduce_87

action_85 (168) = happyShift action_47
action_85 (53) = happyGoto action_60
action_85 (75) = happyGoto action_212
action_85 _ = happyReduce_87

action_86 _ = happyReduce_139

action_87 (168) = happyShift action_47
action_87 (53) = happyGoto action_211
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (168) = happyShift action_47
action_88 (53) = happyGoto action_210
action_88 _ = happyFail (happyExpListPerState 88)

action_89 _ = happyReduce_105

action_90 (168) = happyShift action_47
action_90 (53) = happyGoto action_209
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (169) = happyShift action_180
action_91 (54) = happyGoto action_208
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (166) = happyShift action_207
action_92 (51) = happyGoto action_206
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (165) = happyShift action_205
action_93 (50) = happyGoto action_204
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (120) = happyShift action_135
action_94 (5) = happyGoto action_203
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_110

action_96 _ = happyReduce_111

action_97 _ = happyReduce_112

action_98 _ = happyReduce_113

action_99 _ = happyReduce_114

action_100 _ = happyReduce_116

action_101 _ = happyReduce_118

action_102 _ = happyReduce_119

action_103 _ = happyReduce_117

action_104 _ = happyReduce_120

action_105 _ = happyReduce_121

action_106 _ = happyReduce_122

action_107 _ = happyReduce_123

action_108 (119) = happyShift action_2
action_108 (4) = happyGoto action_202
action_108 _ = happyFail (happyExpListPerState 108)

action_109 _ = happyReduce_125

action_110 _ = happyReduce_126

action_111 _ = happyReduce_127

action_112 _ = happyReduce_128

action_113 _ = happyReduce_129

action_114 (168) = happyShift action_47
action_114 (53) = happyGoto action_201
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (168) = happyShift action_47
action_115 (53) = happyGoto action_200
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (112) = happyShift action_199
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (168) = happyShift action_47
action_117 (53) = happyGoto action_198
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (168) = happyShift action_47
action_118 (53) = happyGoto action_197
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (167) = happyShift action_43
action_119 (52) = happyGoto action_196
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (167) = happyShift action_43
action_120 (52) = happyGoto action_195
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (168) = happyShift action_47
action_121 (53) = happyGoto action_194
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (168) = happyShift action_47
action_122 (53) = happyGoto action_193
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (168) = happyShift action_47
action_123 (53) = happyGoto action_192
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (168) = happyShift action_47
action_124 (53) = happyGoto action_60
action_124 (75) = happyGoto action_191
action_124 _ = happyReduce_87

action_125 (168) = happyShift action_47
action_125 (53) = happyGoto action_190
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (116) = happyShift action_189
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (168) = happyShift action_47
action_127 (53) = happyGoto action_188
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (168) = happyShift action_47
action_128 (53) = happyGoto action_187
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (101) = happyShift action_186
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (103) = happyShift action_184
action_130 (163) = happyShift action_185
action_130 (48) = happyGoto action_183
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (118) = happyShift action_182
action_131 _ = happyFail (happyExpListPerState 131)

action_132 (104) = happyShift action_181
action_132 _ = happyReduce_100

action_133 (169) = happyShift action_180
action_133 (54) = happyGoto action_179
action_133 _ = happyFail (happyExpListPerState 133)

action_134 (168) = happyShift action_47
action_134 (53) = happyGoto action_60
action_134 (75) = happyGoto action_178
action_134 _ = happyReduce_87

action_135 _ = happyReduce_2

action_136 _ = happyReduce_3

action_137 _ = happyReduce_4

action_138 _ = happyReduce_5

action_139 _ = happyReduce_6

action_140 _ = happyReduce_7

action_141 _ = happyReduce_8

action_142 _ = happyReduce_9

action_143 _ = happyReduce_10

action_144 _ = happyReduce_11

action_145 _ = happyReduce_12

action_146 _ = happyReduce_13

action_147 _ = happyReduce_14

action_148 _ = happyReduce_15

action_149 _ = happyReduce_16

action_150 _ = happyReduce_17

action_151 _ = happyReduce_18

action_152 _ = happyReduce_19

action_153 _ = happyReduce_20

action_154 _ = happyReduce_21

action_155 _ = happyReduce_22

action_156 _ = happyReduce_23

action_157 _ = happyReduce_24

action_158 _ = happyReduce_25

action_159 _ = happyReduce_26

action_160 _ = happyReduce_27

action_161 _ = happyReduce_28

action_162 _ = happyReduce_29

action_163 _ = happyReduce_30

action_164 _ = happyReduce_31

action_165 _ = happyReduce_32

action_166 _ = happyReduce_33

action_167 _ = happyReduce_34

action_168 _ = happyReduce_35

action_169 _ = happyReduce_36

action_170 _ = happyReduce_37

action_171 _ = happyReduce_38

action_172 _ = happyReduce_39

action_173 _ = happyReduce_40

action_174 _ = happyReduce_41

action_175 _ = happyReduce_42

action_176 _ = happyReduce_43

action_177 _ = happyReduce_44

action_178 (98) = happyShift action_251
action_178 _ = happyFail (happyExpListPerState 178)

action_179 (97) = happyShift action_250
action_179 _ = happyFail (happyExpListPerState 179)

action_180 _ = happyReduce_51

action_181 (89) = happyShift action_133
action_181 (97) = happyShift action_134
action_181 (120) = happyShift action_135
action_181 (121) = happyShift action_136
action_181 (122) = happyShift action_137
action_181 (123) = happyShift action_138
action_181 (124) = happyShift action_139
action_181 (125) = happyShift action_140
action_181 (126) = happyShift action_141
action_181 (127) = happyShift action_142
action_181 (128) = happyShift action_143
action_181 (129) = happyShift action_144
action_181 (130) = happyShift action_145
action_181 (131) = happyShift action_146
action_181 (132) = happyShift action_147
action_181 (133) = happyShift action_148
action_181 (134) = happyShift action_149
action_181 (135) = happyShift action_150
action_181 (136) = happyShift action_151
action_181 (137) = happyShift action_152
action_181 (138) = happyShift action_153
action_181 (139) = happyShift action_154
action_181 (140) = happyShift action_155
action_181 (141) = happyShift action_156
action_181 (142) = happyShift action_157
action_181 (143) = happyShift action_158
action_181 (144) = happyShift action_159
action_181 (145) = happyShift action_160
action_181 (146) = happyShift action_161
action_181 (147) = happyShift action_162
action_181 (148) = happyShift action_163
action_181 (149) = happyShift action_164
action_181 (150) = happyShift action_165
action_181 (151) = happyShift action_166
action_181 (152) = happyShift action_167
action_181 (153) = happyShift action_168
action_181 (154) = happyShift action_169
action_181 (155) = happyShift action_170
action_181 (156) = happyShift action_171
action_181 (157) = happyShift action_172
action_181 (158) = happyShift action_173
action_181 (159) = happyShift action_174
action_181 (160) = happyShift action_175
action_181 (161) = happyShift action_176
action_181 (162) = happyShift action_177
action_181 (167) = happyShift action_43
action_181 (168) = happyShift action_47
action_181 (5) = happyGoto action_86
action_181 (6) = happyGoto action_87
action_181 (7) = happyGoto action_88
action_181 (8) = happyGoto action_89
action_181 (9) = happyGoto action_90
action_181 (10) = happyGoto action_91
action_181 (11) = happyGoto action_92
action_181 (12) = happyGoto action_93
action_181 (13) = happyGoto action_94
action_181 (14) = happyGoto action_95
action_181 (15) = happyGoto action_96
action_181 (16) = happyGoto action_97
action_181 (17) = happyGoto action_98
action_181 (18) = happyGoto action_99
action_181 (19) = happyGoto action_100
action_181 (20) = happyGoto action_101
action_181 (21) = happyGoto action_102
action_181 (22) = happyGoto action_103
action_181 (23) = happyGoto action_104
action_181 (24) = happyGoto action_105
action_181 (25) = happyGoto action_106
action_181 (26) = happyGoto action_107
action_181 (27) = happyGoto action_108
action_181 (28) = happyGoto action_109
action_181 (29) = happyGoto action_110
action_181 (30) = happyGoto action_111
action_181 (31) = happyGoto action_112
action_181 (32) = happyGoto action_113
action_181 (33) = happyGoto action_114
action_181 (34) = happyGoto action_115
action_181 (35) = happyGoto action_116
action_181 (36) = happyGoto action_117
action_181 (37) = happyGoto action_118
action_181 (38) = happyGoto action_119
action_181 (39) = happyGoto action_120
action_181 (40) = happyGoto action_121
action_181 (41) = happyGoto action_122
action_181 (42) = happyGoto action_123
action_181 (43) = happyGoto action_124
action_181 (44) = happyGoto action_125
action_181 (45) = happyGoto action_126
action_181 (46) = happyGoto action_127
action_181 (47) = happyGoto action_128
action_181 (52) = happyGoto action_129
action_181 (53) = happyGoto action_130
action_181 (82) = happyGoto action_249
action_181 (83) = happyGoto action_132
action_181 _ = happyReduce_99

action_182 _ = happyReduce_98

action_183 (168) = happyShift action_47
action_183 (53) = happyGoto action_248
action_183 _ = happyFail (happyExpListPerState 183)

action_184 (89) = happyShift action_133
action_184 (97) = happyShift action_134
action_184 (120) = happyShift action_135
action_184 (121) = happyShift action_136
action_184 (122) = happyShift action_137
action_184 (123) = happyShift action_138
action_184 (124) = happyShift action_139
action_184 (125) = happyShift action_140
action_184 (126) = happyShift action_141
action_184 (127) = happyShift action_142
action_184 (128) = happyShift action_143
action_184 (129) = happyShift action_144
action_184 (130) = happyShift action_145
action_184 (131) = happyShift action_146
action_184 (132) = happyShift action_147
action_184 (133) = happyShift action_148
action_184 (134) = happyShift action_149
action_184 (135) = happyShift action_150
action_184 (136) = happyShift action_151
action_184 (137) = happyShift action_152
action_184 (138) = happyShift action_153
action_184 (139) = happyShift action_154
action_184 (140) = happyShift action_155
action_184 (141) = happyShift action_156
action_184 (142) = happyShift action_157
action_184 (143) = happyShift action_158
action_184 (144) = happyShift action_159
action_184 (145) = happyShift action_160
action_184 (146) = happyShift action_161
action_184 (147) = happyShift action_162
action_184 (148) = happyShift action_163
action_184 (149) = happyShift action_164
action_184 (150) = happyShift action_165
action_184 (151) = happyShift action_166
action_184 (152) = happyShift action_167
action_184 (153) = happyShift action_168
action_184 (154) = happyShift action_169
action_184 (155) = happyShift action_170
action_184 (156) = happyShift action_171
action_184 (157) = happyShift action_172
action_184 (158) = happyShift action_173
action_184 (159) = happyShift action_174
action_184 (160) = happyShift action_175
action_184 (161) = happyShift action_176
action_184 (162) = happyShift action_177
action_184 (167) = happyShift action_43
action_184 (168) = happyShift action_47
action_184 (5) = happyGoto action_86
action_184 (6) = happyGoto action_87
action_184 (7) = happyGoto action_88
action_184 (8) = happyGoto action_89
action_184 (9) = happyGoto action_90
action_184 (10) = happyGoto action_91
action_184 (11) = happyGoto action_92
action_184 (12) = happyGoto action_93
action_184 (13) = happyGoto action_94
action_184 (14) = happyGoto action_95
action_184 (15) = happyGoto action_96
action_184 (16) = happyGoto action_97
action_184 (17) = happyGoto action_98
action_184 (18) = happyGoto action_99
action_184 (19) = happyGoto action_100
action_184 (20) = happyGoto action_101
action_184 (21) = happyGoto action_102
action_184 (22) = happyGoto action_103
action_184 (23) = happyGoto action_104
action_184 (24) = happyGoto action_105
action_184 (25) = happyGoto action_106
action_184 (26) = happyGoto action_107
action_184 (27) = happyGoto action_108
action_184 (28) = happyGoto action_109
action_184 (29) = happyGoto action_110
action_184 (30) = happyGoto action_111
action_184 (31) = happyGoto action_112
action_184 (32) = happyGoto action_113
action_184 (33) = happyGoto action_114
action_184 (34) = happyGoto action_115
action_184 (35) = happyGoto action_116
action_184 (36) = happyGoto action_117
action_184 (37) = happyGoto action_118
action_184 (38) = happyGoto action_119
action_184 (39) = happyGoto action_120
action_184 (40) = happyGoto action_121
action_184 (41) = happyGoto action_122
action_184 (42) = happyGoto action_123
action_184 (43) = happyGoto action_124
action_184 (44) = happyGoto action_125
action_184 (45) = happyGoto action_126
action_184 (46) = happyGoto action_127
action_184 (47) = happyGoto action_128
action_184 (52) = happyGoto action_129
action_184 (53) = happyGoto action_130
action_184 (83) = happyGoto action_247
action_184 _ = happyFail (happyExpListPerState 184)

action_185 _ = happyReduce_45

action_186 (167) = happyShift action_43
action_186 (52) = happyGoto action_246
action_186 _ = happyFail (happyExpListPerState 186)

action_187 _ = happyReduce_152

action_188 _ = happyReduce_151

action_189 (168) = happyShift action_47
action_189 (53) = happyGoto action_243
action_189 (87) = happyGoto action_244
action_189 (88) = happyGoto action_245
action_189 _ = happyReduce_162

action_190 (97) = happyShift action_242
action_190 _ = happyFail (happyExpListPerState 190)

action_191 (109) = happyShift action_241
action_191 _ = happyFail (happyExpListPerState 191)

action_192 (109) = happyShift action_240
action_192 _ = happyFail (happyExpListPerState 192)

action_193 (111) = happyShift action_239
action_193 _ = happyFail (happyExpListPerState 193)

action_194 (112) = happyShift action_238
action_194 _ = happyFail (happyExpListPerState 194)

action_195 (113) = happyShift action_237
action_195 _ = happyFail (happyExpListPerState 195)

action_196 (101) = happyShift action_236
action_196 _ = happyFail (happyExpListPerState 196)

action_197 (113) = happyShift action_235
action_197 _ = happyFail (happyExpListPerState 197)

action_198 (113) = happyShift action_234
action_198 _ = happyFail (happyExpListPerState 198)

action_199 (116) = happyShift action_233
action_199 _ = happyFail (happyExpListPerState 199)

action_200 (114) = happyShift action_232
action_200 _ = happyFail (happyExpListPerState 200)

action_201 (112) = happyShift action_231
action_201 _ = happyFail (happyExpListPerState 201)

action_202 _ = happyReduce_124

action_203 _ = happyReduce_109

action_204 _ = happyReduce_115

action_205 _ = happyReduce_47

action_206 _ = happyReduce_108

action_207 _ = happyReduce_48

action_208 _ = happyReduce_107

action_209 (97) = happyShift action_230
action_209 _ = happyFail (happyExpListPerState 209)

action_210 _ = happyReduce_104

action_211 _ = happyReduce_103

action_212 _ = happyReduce_89

action_213 (98) = happyShift action_229
action_213 _ = happyFail (happyExpListPerState 213)

action_214 (169) = happyShift action_180
action_214 (54) = happyGoto action_228
action_214 _ = happyFail (happyExpListPerState 214)

action_215 (104) = happyShift action_227
action_215 _ = happyReduce_78

action_216 (118) = happyShift action_226
action_216 _ = happyFail (happyExpListPerState 216)

action_217 (105) = happyShift action_225
action_217 _ = happyFail (happyExpListPerState 217)

action_218 (168) = happyShift action_47
action_218 (53) = happyGoto action_60
action_218 (75) = happyGoto action_224
action_218 _ = happyReduce_87

action_219 _ = happyReduce_64

action_220 (104) = happyShift action_223
action_220 _ = happyReduce_68

action_221 (118) = happyShift action_222
action_221 _ = happyFail (happyExpListPerState 221)

action_222 _ = happyReduce_63

action_223 (167) = happyShift action_43
action_223 (52) = happyGoto action_219
action_223 (60) = happyGoto action_220
action_223 (62) = happyGoto action_276
action_223 _ = happyFail (happyExpListPerState 223)

action_224 (106) = happyShift action_275
action_224 _ = happyFail (happyExpListPerState 224)

action_225 (116) = happyShift action_59
action_225 (81) = happyGoto action_274
action_225 _ = happyFail (happyExpListPerState 225)

action_226 _ = happyReduce_73

action_227 (167) = happyShift action_43
action_227 (52) = happyGoto action_214
action_227 (67) = happyGoto action_215
action_227 (69) = happyGoto action_273
action_227 _ = happyFail (happyExpListPerState 227)

action_228 _ = happyReduce_74

action_229 _ = happyReduce_97

action_230 (168) = happyShift action_47
action_230 (53) = happyGoto action_60
action_230 (75) = happyGoto action_272
action_230 _ = happyReduce_87

action_231 (116) = happyShift action_271
action_231 _ = happyFail (happyExpListPerState 231)

action_232 (116) = happyShift action_59
action_232 (81) = happyGoto action_270
action_232 _ = happyFail (happyExpListPerState 232)

action_233 (167) = happyShift action_43
action_233 (52) = happyGoto action_267
action_233 (84) = happyGoto action_268
action_233 (86) = happyGoto action_269
action_233 _ = happyReduce_158

action_234 (168) = happyShift action_47
action_234 (53) = happyGoto action_266
action_234 _ = happyFail (happyExpListPerState 234)

action_235 (168) = happyShift action_47
action_235 (53) = happyGoto action_265
action_235 _ = happyFail (happyExpListPerState 235)

action_236 (167) = happyShift action_43
action_236 (52) = happyGoto action_264
action_236 _ = happyFail (happyExpListPerState 236)

action_237 (168) = happyShift action_47
action_237 (53) = happyGoto action_263
action_237 _ = happyFail (happyExpListPerState 237)

action_238 (116) = happyShift action_262
action_238 _ = happyFail (happyExpListPerState 238)

action_239 (168) = happyShift action_47
action_239 (53) = happyGoto action_261
action_239 _ = happyFail (happyExpListPerState 239)

action_240 (116) = happyShift action_260
action_240 _ = happyFail (happyExpListPerState 240)

action_241 (116) = happyShift action_259
action_241 _ = happyFail (happyExpListPerState 241)

action_242 (168) = happyShift action_47
action_242 (53) = happyGoto action_60
action_242 (75) = happyGoto action_258
action_242 _ = happyReduce_87

action_243 (100) = happyShift action_257
action_243 _ = happyFail (happyExpListPerState 243)

action_244 (104) = happyShift action_256
action_244 _ = happyReduce_163

action_245 (118) = happyShift action_255
action_245 _ = happyFail (happyExpListPerState 245)

action_246 (97) = happyShift action_254
action_246 (168) = happyShift action_47
action_246 (53) = happyGoto action_253
action_246 _ = happyReduce_130

action_247 _ = happyReduce_102

action_248 _ = happyReduce_149

action_249 _ = happyReduce_101

action_250 (168) = happyShift action_47
action_250 (53) = happyGoto action_252
action_250 _ = happyFail (happyExpListPerState 250)

action_251 _ = happyReduce_137

action_252 (98) = happyShift action_293
action_252 _ = happyFail (happyExpListPerState 252)

action_253 _ = happyReduce_135

action_254 (168) = happyShift action_47
action_254 (53) = happyGoto action_60
action_254 (75) = happyGoto action_292
action_254 _ = happyReduce_87

action_255 _ = happyReduce_150

action_256 (168) = happyShift action_47
action_256 (53) = happyGoto action_243
action_256 (87) = happyGoto action_244
action_256 (88) = happyGoto action_291
action_256 _ = happyReduce_162

action_257 (116) = happyShift action_59
action_257 (81) = happyGoto action_290
action_257 _ = happyFail (happyExpListPerState 257)

action_258 (117) = happyShift action_289
action_258 _ = happyFail (happyExpListPerState 258)

action_259 (115) = happyShift action_288
action_259 _ = happyFail (happyExpListPerState 259)

action_260 (168) = happyShift action_47
action_260 (53) = happyGoto action_287
action_260 _ = happyFail (happyExpListPerState 260)

action_261 (168) = happyShift action_47
action_261 (53) = happyGoto action_286
action_261 _ = happyFail (happyExpListPerState 261)

action_262 (167) = happyShift action_43
action_262 (52) = happyGoto action_267
action_262 (84) = happyGoto action_268
action_262 (86) = happyGoto action_285
action_262 _ = happyReduce_158

action_263 _ = happyReduce_143

action_264 (113) = happyShift action_284
action_264 _ = happyFail (happyExpListPerState 264)

action_265 _ = happyReduce_141

action_266 _ = happyReduce_140

action_267 (101) = happyShift action_283
action_267 _ = happyFail (happyExpListPerState 267)

action_268 (104) = happyShift action_282
action_268 _ = happyReduce_159

action_269 (118) = happyShift action_281
action_269 _ = happyFail (happyExpListPerState 269)

action_270 (110) = happyShift action_280
action_270 _ = happyFail (happyExpListPerState 270)

action_271 (167) = happyShift action_43
action_271 (52) = happyGoto action_267
action_271 (84) = happyGoto action_268
action_271 (86) = happyGoto action_279
action_271 _ = happyReduce_158

action_272 (98) = happyShift action_278
action_272 _ = happyFail (happyExpListPerState 272)

action_273 _ = happyReduce_79

action_274 _ = happyReduce_94

action_275 (168) = happyShift action_47
action_275 (53) = happyGoto action_60
action_275 (75) = happyGoto action_277
action_275 _ = happyReduce_87

action_276 _ = happyReduce_69

action_277 (98) = happyShift action_304
action_277 _ = happyFail (happyExpListPerState 277)

action_278 _ = happyReduce_106

action_279 (118) = happyShift action_303
action_279 _ = happyFail (happyExpListPerState 279)

action_280 (116) = happyShift action_59
action_280 (81) = happyGoto action_302
action_280 _ = happyFail (happyExpListPerState 280)

action_281 _ = happyReduce_134

action_282 (167) = happyShift action_43
action_282 (52) = happyGoto action_267
action_282 (84) = happyGoto action_268
action_282 (86) = happyGoto action_301
action_282 _ = happyReduce_158

action_283 (167) = happyShift action_43
action_283 (52) = happyGoto action_300
action_283 _ = happyFail (happyExpListPerState 283)

action_284 (168) = happyShift action_47
action_284 (53) = happyGoto action_299
action_284 _ = happyFail (happyExpListPerState 284)

action_285 (118) = happyShift action_298
action_285 _ = happyFail (happyExpListPerState 285)

action_286 _ = happyReduce_145

action_287 (115) = happyShift action_297
action_287 _ = happyFail (happyExpListPerState 287)

action_288 (107) = happyShift action_296
action_288 _ = happyFail (happyExpListPerState 288)

action_289 (168) = happyShift action_47
action_289 (53) = happyGoto action_60
action_289 (75) = happyGoto action_295
action_289 _ = happyReduce_87

action_290 _ = happyReduce_161

action_291 _ = happyReduce_164

action_292 (98) = happyShift action_294
action_292 _ = happyFail (happyExpListPerState 292)

action_293 _ = happyReduce_138

action_294 (168) = happyShift action_47
action_294 (53) = happyGoto action_311
action_294 _ = happyReduce_131

action_295 (106) = happyShift action_310
action_295 _ = happyFail (happyExpListPerState 295)

action_296 (168) = happyShift action_47
action_296 (53) = happyGoto action_60
action_296 (75) = happyGoto action_309
action_296 _ = happyReduce_87

action_297 (168) = happyShift action_47
action_297 (53) = happyGoto action_60
action_297 (75) = happyGoto action_308
action_297 _ = happyReduce_87

action_298 _ = happyReduce_144

action_299 _ = happyReduce_142

action_300 (97) = happyShift action_306
action_300 (102) = happyShift action_307
action_300 _ = happyFail (happyExpListPerState 300)

action_301 _ = happyReduce_160

action_302 _ = happyReduce_133

action_303 _ = happyReduce_132

action_304 (105) = happyShift action_305
action_304 _ = happyFail (happyExpListPerState 304)

action_305 (116) = happyShift action_59
action_305 (81) = happyGoto action_317
action_305 _ = happyFail (happyExpListPerState 305)

action_306 (168) = happyShift action_47
action_306 (53) = happyGoto action_60
action_306 (75) = happyGoto action_316
action_306 _ = happyReduce_87

action_307 (116) = happyShift action_59
action_307 (81) = happyGoto action_315
action_307 _ = happyFail (happyExpListPerState 307)

action_308 (102) = happyShift action_314
action_308 _ = happyFail (happyExpListPerState 308)

action_309 (108) = happyShift action_313
action_309 _ = happyFail (happyExpListPerState 309)

action_310 (168) = happyShift action_47
action_310 (53) = happyGoto action_60
action_310 (75) = happyGoto action_312
action_310 _ = happyReduce_87

action_311 _ = happyReduce_136

action_312 (98) = happyShift action_321
action_312 _ = happyFail (happyExpListPerState 312)

action_313 (102) = happyShift action_320
action_313 _ = happyFail (happyExpListPerState 313)

action_314 (116) = happyShift action_59
action_314 (81) = happyGoto action_319
action_314 _ = happyFail (happyExpListPerState 314)

action_315 _ = happyReduce_153

action_316 (98) = happyShift action_318
action_316 _ = happyFail (happyExpListPerState 316)

action_317 _ = happyReduce_86

action_318 (102) = happyShift action_324
action_318 _ = happyFail (happyExpListPerState 318)

action_319 (104) = happyShift action_323
action_319 _ = happyFail (happyExpListPerState 319)

action_320 (116) = happyShift action_59
action_320 (81) = happyGoto action_322
action_320 _ = happyFail (happyExpListPerState 320)

action_321 _ = happyReduce_148

action_322 (104) = happyShift action_327
action_322 _ = happyFail (happyExpListPerState 322)

action_323 (168) = happyShift action_47
action_323 (53) = happyGoto action_326
action_323 _ = happyFail (happyExpListPerState 323)

action_324 (116) = happyShift action_59
action_324 (81) = happyGoto action_325
action_324 _ = happyFail (happyExpListPerState 324)

action_325 _ = happyReduce_154

action_326 (115) = happyShift action_329
action_326 _ = happyFail (happyExpListPerState 326)

action_327 (115) = happyShift action_328
action_327 _ = happyFail (happyExpListPerState 327)

action_328 (107) = happyShift action_331
action_328 _ = happyFail (happyExpListPerState 328)

action_329 (168) = happyShift action_47
action_329 (53) = happyGoto action_60
action_329 (75) = happyGoto action_330
action_329 _ = happyReduce_87

action_330 (102) = happyShift action_333
action_330 _ = happyFail (happyExpListPerState 330)

action_331 (168) = happyShift action_47
action_331 (53) = happyGoto action_60
action_331 (75) = happyGoto action_332
action_331 _ = happyReduce_87

action_332 (108) = happyShift action_335
action_332 _ = happyFail (happyExpListPerState 332)

action_333 (116) = happyShift action_59
action_333 (81) = happyGoto action_334
action_333 _ = happyFail (happyExpListPerState 333)

action_334 (118) = happyShift action_337
action_334 _ = happyFail (happyExpListPerState 334)

action_335 (102) = happyShift action_336
action_335 _ = happyFail (happyExpListPerState 335)

action_336 (116) = happyShift action_59
action_336 (81) = happyGoto action_338
action_336 _ = happyFail (happyExpListPerState 336)

action_337 _ = happyReduce_146

action_338 (118) = happyShift action_339
action_338 _ = happyFail (happyExpListPerState 338)

action_339 _ = happyReduce_147

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn4
		 ((read (happy_var_1)) :: Integer
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (MplAsmLanguage.AbsMPLASM.Store (mkPosToken happy_var_1)
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (MplAsmLanguage.AbsMPLASM.Load (mkPosToken happy_var_1)
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  8 happyReduction_5
happyReduction_5 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (MplAsmLanguage.AbsMPLASM.Ret (mkPosToken happy_var_1)
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  9 happyReduction_6
happyReduction_6 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (MplAsmLanguage.AbsMPLASM.Call (mkPosToken happy_var_1)
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  10 happyReduction_7
happyReduction_7 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (MplAsmLanguage.AbsMPLASM.CInt (mkPosToken happy_var_1)
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  11 happyReduction_8
happyReduction_8 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (MplAsmLanguage.AbsMPLASM.CChar (mkPosToken happy_var_1)
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  12 happyReduction_9
happyReduction_9 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (MplAsmLanguage.AbsMPLASM.CBool (mkPosToken happy_var_1)
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  13 happyReduction_10
happyReduction_10 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn13
		 (MplAsmLanguage.AbsMPLASM.CString (mkPosToken happy_var_1)
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  14 happyReduction_11
happyReduction_11 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (MplAsmLanguage.AbsMPLASM.ToStr (mkPosToken happy_var_1)
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  15 happyReduction_12
happyReduction_12 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn15
		 (MplAsmLanguage.AbsMPLASM.ToInt (mkPosToken happy_var_1)
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  16 happyReduction_13
happyReduction_13 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 (MplAsmLanguage.AbsMPLASM.And (mkPosToken happy_var_1)
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  17 happyReduction_14
happyReduction_14 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (MplAsmLanguage.AbsMPLASM.Or (mkPosToken happy_var_1)
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  18 happyReduction_15
happyReduction_15 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (MplAsmLanguage.AbsMPLASM.Append (mkPosToken happy_var_1)
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  19 happyReduction_16
happyReduction_16 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn19
		 (MplAsmLanguage.AbsMPLASM.Unstring (mkPosToken happy_var_1)
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  20 happyReduction_17
happyReduction_17 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn20
		 (MplAsmLanguage.AbsMPLASM.LeqI (mkPosToken happy_var_1)
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  21 happyReduction_18
happyReduction_18 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 (MplAsmLanguage.AbsMPLASM.EqI (mkPosToken happy_var_1)
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  22 happyReduction_19
happyReduction_19 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (MplAsmLanguage.AbsMPLASM.EqB (mkPosToken happy_var_1)
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  23 happyReduction_20
happyReduction_20 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn23
		 (MplAsmLanguage.AbsMPLASM.LeqC (mkPosToken happy_var_1)
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  24 happyReduction_21
happyReduction_21 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn24
		 (MplAsmLanguage.AbsMPLASM.EqC (mkPosToken happy_var_1)
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  25 happyReduction_22
happyReduction_22 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn25
		 (MplAsmLanguage.AbsMPLASM.Leqs (mkPosToken happy_var_1)
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  26 happyReduction_23
happyReduction_23 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (MplAsmLanguage.AbsMPLASM.Eqs (mkPosToken happy_var_1)
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  27 happyReduction_24
happyReduction_24 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (MplAsmLanguage.AbsMPLASM.ConcatS (mkPosToken happy_var_1)
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  28 happyReduction_25
happyReduction_25 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn28
		 (MplAsmLanguage.AbsMPLASM.Add (mkPosToken happy_var_1)
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  29 happyReduction_26
happyReduction_26 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (MplAsmLanguage.AbsMPLASM.Subtract (mkPosToken happy_var_1)
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  30 happyReduction_27
happyReduction_27 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn30
		 (MplAsmLanguage.AbsMPLASM.Mul (mkPosToken happy_var_1)
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  31 happyReduction_28
happyReduction_28 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn31
		 (MplAsmLanguage.AbsMPLASM.Quot (mkPosToken happy_var_1)
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  32 happyReduction_29
happyReduction_29 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn32
		 (MplAsmLanguage.AbsMPLASM.Rem (mkPosToken happy_var_1)
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  33 happyReduction_30
happyReduction_30 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn33
		 (MplAsmLanguage.AbsMPLASM.Case (mkPosToken happy_var_1)
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  34 happyReduction_31
happyReduction_31 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn34
		 (MplAsmLanguage.AbsMPLASM.If (mkPosToken happy_var_1)
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  35 happyReduction_32
happyReduction_32 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn35
		 (MplAsmLanguage.AbsMPLASM.Rec (mkPosToken happy_var_1)
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  36 happyReduction_33
happyReduction_33 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (MplAsmLanguage.AbsMPLASM.Get (mkPosToken happy_var_1)
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  37 happyReduction_34
happyReduction_34 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn37
		 (MplAsmLanguage.AbsMPLASM.Put (mkPosToken happy_var_1)
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  38 happyReduction_35
happyReduction_35 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn38
		 (MplAsmLanguage.AbsMPLASM.Hput (mkPosToken happy_var_1)
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  39 happyReduction_36
happyReduction_36 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn39
		 (MplAsmLanguage.AbsMPLASM.Shput (mkPosToken happy_var_1)
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  40 happyReduction_37
happyReduction_37 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn40
		 (MplAsmLanguage.AbsMPLASM.Hcase (mkPosToken happy_var_1)
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  41 happyReduction_38
happyReduction_38 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn41
		 (MplAsmLanguage.AbsMPLASM.Split (mkPosToken happy_var_1)
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  42 happyReduction_39
happyReduction_39 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn42
		 (MplAsmLanguage.AbsMPLASM.Fork (mkPosToken happy_var_1)
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  43 happyReduction_40
happyReduction_40 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn43
		 (MplAsmLanguage.AbsMPLASM.Plug (mkPosToken happy_var_1)
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  44 happyReduction_41
happyReduction_41 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn44
		 (MplAsmLanguage.AbsMPLASM.Run (mkPosToken happy_var_1)
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  45 happyReduction_42
happyReduction_42 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn45
		 (MplAsmLanguage.AbsMPLASM.Race (mkPosToken happy_var_1)
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  46 happyReduction_43
happyReduction_43 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn46
		 (MplAsmLanguage.AbsMPLASM.Close (mkPosToken happy_var_1)
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  47 happyReduction_44
happyReduction_44 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn47
		 (MplAsmLanguage.AbsMPLASM.Halt (mkPosToken happy_var_1)
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  48 happyReduction_45
happyReduction_45 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn48
		 (MplAsmLanguage.AbsMPLASM.Ch_Id (mkPosToken happy_var_1)
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  49 happyReduction_46
happyReduction_46 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (MplAsmLanguage.AbsMPLASM.Main_run (mkPosToken happy_var_1)
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  50 happyReduction_47
happyReduction_47 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn50
		 (MplAsmLanguage.AbsMPLASM.BBool (mkPosToken happy_var_1)
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  51 happyReduction_48
happyReduction_48 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn51
		 (MplAsmLanguage.AbsMPLASM.Character (mkPosToken happy_var_1)
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  52 happyReduction_49
happyReduction_49 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn52
		 (MplAsmLanguage.AbsMPLASM.UIdent (mkPosToken happy_var_1)
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  53 happyReduction_50
happyReduction_50 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn53
		 (MplAsmLanguage.AbsMPLASM.PIdent (mkPosToken happy_var_1)
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  54 happyReduction_51
happyReduction_51 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn54
		 (MplAsmLanguage.AbsMPLASM.PInteger (mkPosToken happy_var_1)
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  55 happyReduction_52
happyReduction_52 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn55
		 (MplAsmLanguage.AbsMPLASM.IIdent (mkPosToken happy_var_1)
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  56 happyReduction_53
happyReduction_53 (HappyAbsSyn79  happy_var_2)
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn56
		 (MplAsmLanguage.AbsMPLASM.AMPLCODE happy_var_1 happy_var_2
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  57 happyReduction_54
happyReduction_54 (HappyAbsSyn63  happy_var_1)
	 =  HappyAbsSyn57
		 (MplAsmLanguage.AbsMPLASM.IMPORT_CONSTRUCT happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  57 happyReduction_55
happyReduction_55 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn57
		 (MplAsmLanguage.AbsMPLASM.PROTOCOL_CONSTRUCT happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  57 happyReduction_56
happyReduction_56 (HappyAbsSyn71  happy_var_1)
	 =  HappyAbsSyn57
		 (MplAsmLanguage.AbsMPLASM.COPROTOCOL_CONSTRUCT happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  57 happyReduction_57
happyReduction_57 (HappyAbsSyn64  happy_var_1)
	 =  HappyAbsSyn57
		 (MplAsmLanguage.AbsMPLASM.CONSTRUCTOR_CONSTRUCT happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  57 happyReduction_58
happyReduction_58 (HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn57
		 (MplAsmLanguage.AbsMPLASM.DESTRUCTOR_CONSTRUCT happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  57 happyReduction_59
happyReduction_59 (HappyAbsSyn72  happy_var_1)
	 =  HappyAbsSyn57
		 (MplAsmLanguage.AbsMPLASM.PROCESSES_CONSTRUCT happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  57 happyReduction_60
happyReduction_60 (HappyAbsSyn76  happy_var_1)
	 =  HappyAbsSyn57
		 (MplAsmLanguage.AbsMPLASM.FUNCTIONS_CONSTRUCT happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_0  58 happyReduction_61
happyReduction_61  =  HappyAbsSyn58
		 ([]
	)

happyReduce_62 = happySpecReduce_2  58 happyReduction_62
happyReduction_62 (HappyAbsSyn58  happy_var_2)
	(HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn58
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_62 _ _  = notHappyAtAll 

happyReduce_63 = happyReduce 5 59 happyReduction_63
happyReduction_63 (_ `HappyStk`
	(HappyAbsSyn62  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn59
		 (MplAsmLanguage.AbsMPLASM.PROTOCOL_COPROTOCOL_SPEC happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_64 = happySpecReduce_1  60 happyReduction_64
happyReduction_64 (HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn60
		 (MplAsmLanguage.AbsMPLASM.HANDLE_NAME happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_0  61 happyReduction_65
happyReduction_65  =  HappyAbsSyn61
		 ([]
	)

happyReduce_66 = happySpecReduce_1  61 happyReduction_66
happyReduction_66 (HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn61
		 ((:[]) happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  61 happyReduction_67
happyReduction_67 (HappyAbsSyn61  happy_var_3)
	_
	(HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn61
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  62 happyReduction_68
happyReduction_68 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn62
		 ((:[]) happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  62 happyReduction_69
happyReduction_69 (HappyAbsSyn62  happy_var_3)
	_
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn62
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_2  63 happyReduction_70
happyReduction_70 (HappyAbsSyn55  happy_var_2)
	_
	 =  HappyAbsSyn63
		 (MplAsmLanguage.AbsMPLASM.IMPORT happy_var_2
	)
happyReduction_70 _ _  = notHappyAtAll 

happyReduce_71 = happyReduce 5 64 happyReduction_71
happyReduction_71 (_ `HappyStk`
	(HappyAbsSyn68  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn64
		 (MplAsmLanguage.AbsMPLASM.CONSTRUCTORS happy_var_4
	) `HappyStk` happyRest

happyReduce_72 = happyReduce 5 65 happyReduction_72
happyReduction_72 (_ `HappyStk`
	(HappyAbsSyn68  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn65
		 (MplAsmLanguage.AbsMPLASM.DESTRUCTORS happy_var_4
	) `HappyStk` happyRest

happyReduce_73 = happyReduce 5 66 happyReduction_73
happyReduction_73 (_ `HappyStk`
	(HappyAbsSyn69  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn66
		 (MplAsmLanguage.AbsMPLASM.STRUCT_SPEC happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_74 = happySpecReduce_2  67 happyReduction_74
happyReduction_74 (HappyAbsSyn54  happy_var_2)
	(HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn67
		 (MplAsmLanguage.AbsMPLASM.STRUCT happy_var_1 happy_var_2
	)
happyReduction_74 _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_0  68 happyReduction_75
happyReduction_75  =  HappyAbsSyn68
		 ([]
	)

happyReduce_76 = happySpecReduce_1  68 happyReduction_76
happyReduction_76 (HappyAbsSyn66  happy_var_1)
	 =  HappyAbsSyn68
		 ((:[]) happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  68 happyReduction_77
happyReduction_77 (HappyAbsSyn68  happy_var_3)
	_
	(HappyAbsSyn66  happy_var_1)
	 =  HappyAbsSyn68
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  69 happyReduction_78
happyReduction_78 (HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn69
		 ((:[]) happy_var_1
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_3  69 happyReduction_79
happyReduction_79 (HappyAbsSyn69  happy_var_3)
	_
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn69
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happyReduce 5 70 happyReduction_80
happyReduction_80 (_ `HappyStk`
	(HappyAbsSyn61  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn70
		 (MplAsmLanguage.AbsMPLASM.PROTOCOLS happy_var_4
	) `HappyStk` happyRest

happyReduce_81 = happyReduce 5 71 happyReduction_81
happyReduction_81 (_ `HappyStk`
	(HappyAbsSyn61  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn71
		 (MplAsmLanguage.AbsMPLASM.COPROTOCOLS happy_var_4
	) `HappyStk` happyRest

happyReduce_82 = happyReduce 5 72 happyReduction_82
happyReduction_82 (_ `HappyStk`
	(HappyAbsSyn73  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn72
		 (MplAsmLanguage.AbsMPLASM.PROCESSES happy_var_4
	) `HappyStk` happyRest

happyReduce_83 = happySpecReduce_0  73 happyReduction_83
happyReduction_83  =  HappyAbsSyn73
		 ([]
	)

happyReduce_84 = happySpecReduce_1  73 happyReduction_84
happyReduction_84 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn73
		 ((:[]) happy_var_1
	)
happyReduction_84 _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_3  73 happyReduction_85
happyReduction_85 (HappyAbsSyn73  happy_var_3)
	_
	(HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn73
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_85 _ _ _  = notHappyAtAll 

happyReduce_86 = happyReduce 10 74 happyReduction_86
happyReduction_86 ((HappyAbsSyn81  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn75  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn75  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn75  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn74
		 (MplAsmLanguage.AbsMPLASM.PROCESS_SPEC happy_var_1 happy_var_3 happy_var_5 happy_var_7 happy_var_10
	) `HappyStk` happyRest

happyReduce_87 = happySpecReduce_0  75 happyReduction_87
happyReduction_87  =  HappyAbsSyn75
		 ([]
	)

happyReduce_88 = happySpecReduce_1  75 happyReduction_88
happyReduction_88 (HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn75
		 ((:[]) happy_var_1
	)
happyReduction_88 _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_3  75 happyReduction_89
happyReduction_89 (HappyAbsSyn75  happy_var_3)
	_
	(HappyAbsSyn53  happy_var_1)
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
		 (MplAsmLanguage.AbsMPLASM.FUNCTIONS happy_var_4
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
	(HappyAbsSyn53  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn78
		 (MplAsmLanguage.AbsMPLASM.FUNCTION_SPEC happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_95 = happyReduce 4 79 happyReduction_95
happyReduction_95 ((HappyAbsSyn81  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn80  happy_var_2) `HappyStk`
	(HappyAbsSyn49  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn79
		 (MplAsmLanguage.AbsMPLASM.MAIN happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_96 = happySpecReduce_0  79 happyReduction_96
happyReduction_96  =  HappyAbsSyn79
		 (MplAsmLanguage.AbsMPLASM.NO_MAIN
	)

happyReduce_97 = happyReduce 6 80 happyReduction_97
happyReduction_97 (_ `HappyStk`
	(HappyAbsSyn75  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn75  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn80
		 (MplAsmLanguage.AbsMPLASM.MAIN_CHANNELS happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_98 = happySpecReduce_3  81 happyReduction_98
happyReduction_98 _
	(HappyAbsSyn82  happy_var_2)
	_
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.Prog happy_var_2
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
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_ASSIGN happy_var_1 happy_var_3
	)
happyReduction_102 _ _ _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_2  83 happyReduction_103
happyReduction_103 (HappyAbsSyn53  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_STORE happy_var_1 happy_var_2
	)
happyReduction_103 _ _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_2  83 happyReduction_104
happyReduction_104 (HappyAbsSyn53  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_LOAD happy_var_1 happy_var_2
	)
happyReduction_104 _ _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_1  83 happyReduction_105
happyReduction_105 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_RET happy_var_1
	)
happyReduction_105 _  = notHappyAtAll 

happyReduce_106 = happyReduce 5 83 happyReduction_106
happyReduction_106 (_ `HappyStk`
	(HappyAbsSyn75  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_2) `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_CALL_FUN happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_107 = happySpecReduce_2  83 happyReduction_107
happyReduction_107 (HappyAbsSyn54  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_INT happy_var_1 happy_var_2
	)
happyReduction_107 _ _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_2  83 happyReduction_108
happyReduction_108 (HappyAbsSyn51  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_CHAR happy_var_1 happy_var_2
	)
happyReduction_108 _ _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_2  83 happyReduction_109
happyReduction_109 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_STRING happy_var_1 happy_var_2
	)
happyReduction_109 _ _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_1  83 happyReduction_110
happyReduction_110 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_TOSTR happy_var_1
	)
happyReduction_110 _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_1  83 happyReduction_111
happyReduction_111 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_TOINT happy_var_1
	)
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_1  83 happyReduction_112
happyReduction_112 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_AND happy_var_1
	)
happyReduction_112 _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_1  83 happyReduction_113
happyReduction_113 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_OR happy_var_1
	)
happyReduction_113 _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_1  83 happyReduction_114
happyReduction_114 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_APPEND happy_var_1
	)
happyReduction_114 _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_2  83 happyReduction_115
happyReduction_115 (HappyAbsSyn50  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_BOOL happy_var_1 happy_var_2
	)
happyReduction_115 _ _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_1  83 happyReduction_116
happyReduction_116 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_UNSTRING happy_var_1
	)
happyReduction_116 _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_1  83 happyReduction_117
happyReduction_117 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_EQB happy_var_1
	)
happyReduction_117 _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_1  83 happyReduction_118
happyReduction_118 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_LEQ happy_var_1
	)
happyReduction_118 _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_1  83 happyReduction_119
happyReduction_119 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_EQI happy_var_1
	)
happyReduction_119 _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_1  83 happyReduction_120
happyReduction_120 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_LEQC happy_var_1
	)
happyReduction_120 _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_1  83 happyReduction_121
happyReduction_121 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_EQC happy_var_1
	)
happyReduction_121 _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_1  83 happyReduction_122
happyReduction_122 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_LEQS happy_var_1
	)
happyReduction_122 _  = notHappyAtAll 

happyReduce_123 = happySpecReduce_1  83 happyReduction_123
happyReduction_123 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_EQS happy_var_1
	)
happyReduction_123 _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_2  83 happyReduction_124
happyReduction_124 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_CONCAT happy_var_1 happy_var_2
	)
happyReduction_124 _ _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_1  83 happyReduction_125
happyReduction_125 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_ADD happy_var_1
	)
happyReduction_125 _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_1  83 happyReduction_126
happyReduction_126 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_SUB happy_var_1
	)
happyReduction_126 _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_1  83 happyReduction_127
happyReduction_127 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_MUL happy_var_1
	)
happyReduction_127 _  = notHappyAtAll 

happyReduce_128 = happySpecReduce_1  83 happyReduction_128
happyReduction_128 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_DIVQ happy_var_1
	)
happyReduction_128 _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_1  83 happyReduction_129
happyReduction_129 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_DIVR happy_var_1
	)
happyReduction_129 _  = notHappyAtAll 

happyReduce_130 = happySpecReduce_3  83 happyReduction_130
happyReduction_130 (HappyAbsSyn52  happy_var_3)
	_
	(HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_CONSTRUCTOR happy_var_1 happy_var_3
	)
happyReduction_130 _ _ _  = notHappyAtAll 

happyReduce_131 = happyReduce 6 83 happyReduction_131
happyReduction_131 (_ `HappyStk`
	(HappyAbsSyn75  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_CONSTRUCTOR_ARGS happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_132 = happyReduce 6 83 happyReduction_132
happyReduction_132 (_ `HappyStk`
	(HappyAbsSyn86  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_2) `HappyStk`
	(HappyAbsSyn33  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_CASE happy_var_1 happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_133 = happyReduce 6 83 happyReduction_133
happyReduction_133 ((HappyAbsSyn81  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn81  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_2) `HappyStk`
	(HappyAbsSyn34  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_IF happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_134 = happyReduce 5 83 happyReduction_134
happyReduction_134 (_ `HappyStk`
	(HappyAbsSyn86  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_RECORD happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_135 = happyReduce 4 83 happyReduction_135
happyReduction_135 ((HappyAbsSyn53  happy_var_4) `HappyStk`
	(HappyAbsSyn52  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_DEST happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_136 = happyReduce 7 83 happyReduction_136
happyReduction_136 ((HappyAbsSyn53  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn75  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_DEST_ARGS happy_var_1 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_137 = happySpecReduce_3  83 happyReduction_137
happyReduction_137 _
	(HappyAbsSyn75  happy_var_2)
	_
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_PROD happy_var_2
	)
happyReduction_137 _ _ _  = notHappyAtAll 

happyReduce_138 = happyReduce 5 83 happyReduction_138
happyReduction_138 (_ `HappyStk`
	(HappyAbsSyn53  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn54  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_PRODELEM happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_139 = happySpecReduce_1  83 happyReduction_139
happyReduction_139 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_EMSG happy_var_1
	)
happyReduction_139 _  = notHappyAtAll 

happyReduce_140 = happyReduce 4 83 happyReduction_140
happyReduction_140 ((HappyAbsSyn53  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_2) `HappyStk`
	(HappyAbsSyn36  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_GET happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_141 = happyReduce 4 83 happyReduction_141
happyReduction_141 ((HappyAbsSyn53  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_2) `HappyStk`
	(HappyAbsSyn37  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_PUT happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_142 = happyReduce 6 83 happyReduction_142
happyReduction_142 ((HappyAbsSyn53  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_2) `HappyStk`
	(HappyAbsSyn38  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_HPUT happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_143 = happyReduce 4 83 happyReduction_143
happyReduction_143 ((HappyAbsSyn53  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_2) `HappyStk`
	(HappyAbsSyn39  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_SHPUT happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_144 = happyReduce 6 83 happyReduction_144
happyReduction_144 (_ `HappyStk`
	(HappyAbsSyn86  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_2) `HappyStk`
	(HappyAbsSyn40  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_HCASE happy_var_1 happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_145 = happyReduce 5 83 happyReduction_145
happyReduction_145 ((HappyAbsSyn53  happy_var_5) `HappyStk`
	(HappyAbsSyn53  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_2) `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_SPLIT happy_var_1 happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_146 = happyReduce 16 83 happyReduction_146
happyReduction_146 (_ `HappyStk`
	(HappyAbsSyn81  happy_var_15) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn75  happy_var_13) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_11) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn81  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn75  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_2) `HappyStk`
	(HappyAbsSyn42  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_FORK happy_var_1 happy_var_2 happy_var_5 happy_var_7 happy_var_9 happy_var_11 happy_var_13 happy_var_15
	) `HappyStk` happyRest

happyReduce_147 = happyReduce 18 83 happyReduction_147
happyReduction_147 (_ `HappyStk`
	(HappyAbsSyn81  happy_var_17) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn75  happy_var_14) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn81  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn75  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn75  happy_var_2) `HappyStk`
	(HappyAbsSyn43  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_PLUG happy_var_1 happy_var_2 happy_var_7 happy_var_10 happy_var_14 happy_var_17
	) `HappyStk` happyRest

happyReduce_148 = happyReduce 9 83 happyReduction_148
happyReduction_148 (_ `HappyStk`
	(HappyAbsSyn75  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn75  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn75  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_2) `HappyStk`
	(HappyAbsSyn44  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_RUN happy_var_1 happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_149 = happySpecReduce_3  83 happyReduction_149
happyReduction_149 (HappyAbsSyn53  happy_var_3)
	(HappyAbsSyn48  happy_var_2)
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_ID happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_149 _ _ _  = notHappyAtAll 

happyReduce_150 = happyReduce 4 83 happyReduction_150
happyReduction_150 (_ `HappyStk`
	(HappyAbsSyn88  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn45  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_RACE happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_151 = happySpecReduce_2  83 happyReduction_151
happyReduction_151 (HappyAbsSyn53  happy_var_2)
	(HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_CLOSE happy_var_1 happy_var_2
	)
happyReduction_151 _ _  = notHappyAtAll 

happyReduce_152 = happySpecReduce_2  83 happyReduction_152
happyReduction_152 (HappyAbsSyn53  happy_var_2)
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn83
		 (MplAsmLanguage.AbsMPLASM.AC_HALT happy_var_1 happy_var_2
	)
happyReduction_152 _ _  = notHappyAtAll 

happyReduce_153 = happyReduce 5 84 happyReduction_153
happyReduction_153 ((HappyAbsSyn81  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn84
		 (MplAsmLanguage.AbsMPLASM.AC_LABELLED_COMS_NO_ARGS happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_154 = happyReduce 8 84 happyReduction_154
happyReduction_154 ((HappyAbsSyn81  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn75  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn84
		 (MplAsmLanguage.AbsMPLASM.AC_LABELLED_COMS happy_var_1 happy_var_3 happy_var_5 happy_var_8
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
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn87
		 (MplAsmLanguage.AbsMPLASM.AC_RACE_PHRASE happy_var_1 happy_var_3
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

happyNewToken action sts stk [] =
	action 171 171 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 89;
	PT _ (TS _ 2) -> cont 90;
	PT _ (TS _ 3) -> cont 91;
	PT _ (TS _ 4) -> cont 92;
	PT _ (TS _ 5) -> cont 93;
	PT _ (TS _ 6) -> cont 94;
	PT _ (TS _ 7) -> cont 95;
	PT _ (TS _ 8) -> cont 96;
	PT _ (TS _ 9) -> cont 97;
	PT _ (TS _ 10) -> cont 98;
	PT _ (TS _ 11) -> cont 99;
	PT _ (TS _ 12) -> cont 100;
	PT _ (TS _ 13) -> cont 101;
	PT _ (TS _ 14) -> cont 102;
	PT _ (TS _ 15) -> cont 103;
	PT _ (TS _ 16) -> cont 104;
	PT _ (TS _ 17) -> cont 105;
	PT _ (TS _ 18) -> cont 106;
	PT _ (TS _ 19) -> cont 107;
	PT _ (TS _ 20) -> cont 108;
	PT _ (TS _ 21) -> cont 109;
	PT _ (TS _ 22) -> cont 110;
	PT _ (TS _ 23) -> cont 111;
	PT _ (TS _ 24) -> cont 112;
	PT _ (TS _ 25) -> cont 113;
	PT _ (TS _ 26) -> cont 114;
	PT _ (TS _ 27) -> cont 115;
	PT _ (TS _ 28) -> cont 116;
	PT _ (TS _ 29) -> cont 117;
	PT _ (TS _ 30) -> cont 118;
	PT _ (TI happy_dollar_dollar) -> cont 119;
	PT _ (TL happy_dollar_dollar) -> cont 120;
	PT _ (T_Store _) -> cont 121;
	PT _ (T_Load _) -> cont 122;
	PT _ (T_Ret _) -> cont 123;
	PT _ (T_Call _) -> cont 124;
	PT _ (T_CInt _) -> cont 125;
	PT _ (T_CChar _) -> cont 126;
	PT _ (T_CBool _) -> cont 127;
	PT _ (T_CString _) -> cont 128;
	PT _ (T_ToStr _) -> cont 129;
	PT _ (T_ToInt _) -> cont 130;
	PT _ (T_And _) -> cont 131;
	PT _ (T_Or _) -> cont 132;
	PT _ (T_Append _) -> cont 133;
	PT _ (T_Unstring _) -> cont 134;
	PT _ (T_LeqI _) -> cont 135;
	PT _ (T_EqI _) -> cont 136;
	PT _ (T_EqB _) -> cont 137;
	PT _ (T_LeqC _) -> cont 138;
	PT _ (T_EqC _) -> cont 139;
	PT _ (T_Leqs _) -> cont 140;
	PT _ (T_Eqs _) -> cont 141;
	PT _ (T_ConcatS _) -> cont 142;
	PT _ (T_Add _) -> cont 143;
	PT _ (T_Subtract _) -> cont 144;
	PT _ (T_Mul _) -> cont 145;
	PT _ (T_Quot _) -> cont 146;
	PT _ (T_Rem _) -> cont 147;
	PT _ (T_Case _) -> cont 148;
	PT _ (T_If _) -> cont 149;
	PT _ (T_Rec _) -> cont 150;
	PT _ (T_Get _) -> cont 151;
	PT _ (T_Put _) -> cont 152;
	PT _ (T_Hput _) -> cont 153;
	PT _ (T_Shput _) -> cont 154;
	PT _ (T_Hcase _) -> cont 155;
	PT _ (T_Split _) -> cont 156;
	PT _ (T_Fork _) -> cont 157;
	PT _ (T_Plug _) -> cont 158;
	PT _ (T_Run _) -> cont 159;
	PT _ (T_Race _) -> cont 160;
	PT _ (T_Close _) -> cont 161;
	PT _ (T_Halt _) -> cont 162;
	PT _ (T_Ch_Id _) -> cont 163;
	PT _ (T_Main_run _) -> cont 164;
	PT _ (T_BBool _) -> cont 165;
	PT _ (T_Character _) -> cont 166;
	PT _ (T_UIdent _) -> cont 167;
	PT _ (T_PIdent _) -> cont 168;
	PT _ (T_PInteger _) -> cont 169;
	PT _ (T_IIdent _) -> cont 170;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 171 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Either String a -> (a -> Either String b) -> Either String b
happyThen = ((>>=))
happyReturn :: () => a -> Either String a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either String a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Either String a
happyError' = (\(tokens, _) -> happyError tokens)
pAMPLCODE tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn56 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [Token] -> Either String a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

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
