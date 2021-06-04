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
	| HappyAbsSyn22 (MplAsmLanguage.AbsMPLASM.LeqC)
	| HappyAbsSyn23 (MplAsmLanguage.AbsMPLASM.EqC)
	| HappyAbsSyn24 (MplAsmLanguage.AbsMPLASM.Leqs)
	| HappyAbsSyn25 (MplAsmLanguage.AbsMPLASM.Eqs)
	| HappyAbsSyn26 (MplAsmLanguage.AbsMPLASM.ConcatS)
	| HappyAbsSyn27 (MplAsmLanguage.AbsMPLASM.Add)
	| HappyAbsSyn28 (MplAsmLanguage.AbsMPLASM.Subtract)
	| HappyAbsSyn29 (MplAsmLanguage.AbsMPLASM.Mul)
	| HappyAbsSyn30 (MplAsmLanguage.AbsMPLASM.Quot)
	| HappyAbsSyn31 (MplAsmLanguage.AbsMPLASM.Rem)
	| HappyAbsSyn32 (MplAsmLanguage.AbsMPLASM.Case)
	| HappyAbsSyn33 (MplAsmLanguage.AbsMPLASM.If)
	| HappyAbsSyn34 (MplAsmLanguage.AbsMPLASM.Rec)
	| HappyAbsSyn35 (MplAsmLanguage.AbsMPLASM.Get)
	| HappyAbsSyn36 (MplAsmLanguage.AbsMPLASM.Put)
	| HappyAbsSyn37 (MplAsmLanguage.AbsMPLASM.Hput)
	| HappyAbsSyn38 (MplAsmLanguage.AbsMPLASM.Hcase)
	| HappyAbsSyn39 (MplAsmLanguage.AbsMPLASM.Split)
	| HappyAbsSyn40 (MplAsmLanguage.AbsMPLASM.Fork)
	| HappyAbsSyn41 (MplAsmLanguage.AbsMPLASM.Plug)
	| HappyAbsSyn42 (MplAsmLanguage.AbsMPLASM.Run)
	| HappyAbsSyn43 (MplAsmLanguage.AbsMPLASM.Race)
	| HappyAbsSyn44 (MplAsmLanguage.AbsMPLASM.Close)
	| HappyAbsSyn45 (MplAsmLanguage.AbsMPLASM.Halt)
	| HappyAbsSyn46 (MplAsmLanguage.AbsMPLASM.Ch_Id)
	| HappyAbsSyn47 (MplAsmLanguage.AbsMPLASM.Main_run)
	| HappyAbsSyn48 (MplAsmLanguage.AbsMPLASM.BBool)
	| HappyAbsSyn49 (MplAsmLanguage.AbsMPLASM.Character)
	| HappyAbsSyn50 (MplAsmLanguage.AbsMPLASM.UIdent)
	| HappyAbsSyn51 (MplAsmLanguage.AbsMPLASM.PIdent)
	| HappyAbsSyn52 (MplAsmLanguage.AbsMPLASM.PInteger)
	| HappyAbsSyn53 (MplAsmLanguage.AbsMPLASM.IIdent)
	| HappyAbsSyn54 (MplAsmLanguage.AbsMPLASM.AMPLCODE)
	| HappyAbsSyn55 (MplAsmLanguage.AbsMPLASM.AmplConstructs)
	| HappyAbsSyn56 ([MplAsmLanguage.AbsMPLASM.AmplConstructs])
	| HappyAbsSyn57 (MplAsmLanguage.AbsMPLASM.ProtocolCoprotocolSpec)
	| HappyAbsSyn58 (MplAsmLanguage.AbsMPLASM.Handle)
	| HappyAbsSyn59 ([MplAsmLanguage.AbsMPLASM.ProtocolCoprotocolSpec])
	| HappyAbsSyn60 ([MplAsmLanguage.AbsMPLASM.Handle])
	| HappyAbsSyn61 (MplAsmLanguage.AbsMPLASM.Import)
	| HappyAbsSyn62 (MplAsmLanguage.AbsMPLASM.Constructors)
	| HappyAbsSyn63 (MplAsmLanguage.AbsMPLASM.Destructors)
	| HappyAbsSyn64 (MplAsmLanguage.AbsMPLASM.StructorSpec)
	| HappyAbsSyn65 (MplAsmLanguage.AbsMPLASM.Struct)
	| HappyAbsSyn66 ([MplAsmLanguage.AbsMPLASM.StructorSpec])
	| HappyAbsSyn67 ([MplAsmLanguage.AbsMPLASM.Struct])
	| HappyAbsSyn68 (MplAsmLanguage.AbsMPLASM.Protocols)
	| HappyAbsSyn69 (MplAsmLanguage.AbsMPLASM.Coprotocols)
	| HappyAbsSyn70 (MplAsmLanguage.AbsMPLASM.Processes)
	| HappyAbsSyn71 ([MplAsmLanguage.AbsMPLASM.ProcessesSpec])
	| HappyAbsSyn72 (MplAsmLanguage.AbsMPLASM.ProcessesSpec)
	| HappyAbsSyn73 ([MplAsmLanguage.AbsMPLASM.PIdent])
	| HappyAbsSyn74 (MplAsmLanguage.AbsMPLASM.Functions)
	| HappyAbsSyn75 ([MplAsmLanguage.AbsMPLASM.FunctionsSpec])
	| HappyAbsSyn76 (MplAsmLanguage.AbsMPLASM.FunctionsSpec)
	| HappyAbsSyn77 (MplAsmLanguage.AbsMPLASM.Main)
	| HappyAbsSyn78 (MplAsmLanguage.AbsMPLASM.MainChannels)
	| HappyAbsSyn79 (MplAsmLanguage.AbsMPLASM.Coms)
	| HappyAbsSyn80 ([MplAsmLanguage.AbsMPLASM.Com])
	| HappyAbsSyn81 (MplAsmLanguage.AbsMPLASM.Com)
	| HappyAbsSyn82 (MplAsmLanguage.AbsMPLASM.LabelledComs)
	| HappyAbsSyn83 ([MplAsmLanguage.AbsMPLASM.Coms])
	| HappyAbsSyn84 ([MplAsmLanguage.AbsMPLASM.LabelledComs])
	| HappyAbsSyn85 (MplAsmLanguage.AbsMPLASM.RACE_PHRASE)
	| HappyAbsSyn86 ([MplAsmLanguage.AbsMPLASM.RACE_PHRASE])

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
 action_329 :: () => Prelude.Int -> ({-HappyReduction (Either String) = -}
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
 happyReduce_159 :: () => ({-HappyReduction (Either String) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Either String) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Either String) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Either String) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,573) ([0,0,0,0,0,16256,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1016,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2056,0,65524,65535,34815,1,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,8192,0,0,0,128,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,128,16384,65535,65535,6271,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,4112,0,65512,65535,4095,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,512,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pAMPLCODE","Integer","String","Store","Load","Ret","Call","CInt","CChar","CBool","CString","ToStr","ToInt","And","Or","Append","Unstring","LeqI","EqI","LeqC","EqC","Leqs","Eqs","ConcatS","Add","Subtract","Mul","Quot","Rem","Case","If","Rec","Get","Put","Hput","Hcase","Split","Fork","Plug","Run","Race","Close","Halt","Ch_Id","Main_run","BBool","Character","UIdent","PIdent","PInteger","IIdent","AMPLCODE","AmplConstructs","ListAmplConstructs","ProtocolCoprotocolSpec","Handle","ListProtocolCoprotocolSpec","ListHandle","Import","Constructors","Destructors","StructorSpec","Struct","ListStructorSpec","ListStruct","Protocols","Coprotocols","Processes","ListProcessesSpec","ProcessesSpec","ListPIdent","Functions","ListFunctionsSpec","FunctionsSpec","Main","MainChannels","Coms","ListCom","Com","LabelledComs","ListComs","ListLabelledComs","RACE_PHRASE","ListRACE_PHRASE","'#'","'%constructors'","'%coprotocols'","'%destructors'","'%functions'","'%include'","'%processes'","'%protocols'","'('","')'","','","'->'","'.'","':'","':='","';'","'='","'=>'","'['","']'","'as'","'else'","'into'","'of'","'on'","'then'","'with'","'{'","'|'","'}'","L_integ","L_quoted","L_Store","L_Load","L_Ret","L_Call","L_CInt","L_CChar","L_CBool","L_CString","L_ToStr","L_ToInt","L_And","L_Or","L_Append","L_Unstring","L_LeqI","L_EqI","L_LeqC","L_EqC","L_Leqs","L_Eqs","L_ConcatS","L_Add","L_Subtract","L_Mul","L_Quot","L_Rem","L_Case","L_If","L_Rec","L_Get","L_Put","L_Hput","L_Hcase","L_Split","L_Fork","L_Plug","L_Run","L_Race","L_Close","L_Halt","L_Ch_Id","L_Main_run","L_BBool","L_Character","L_UIdent","L_PIdent","L_PInteger","L_IIdent","%eof"]
        bit_start = st Prelude.* 167
        bit_end = (st Prelude.+ 1) Prelude.* 167
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..166]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (88) = happyShift action_13
action_0 (89) = happyShift action_14
action_0 (90) = happyShift action_15
action_0 (91) = happyShift action_16
action_0 (92) = happyShift action_17
action_0 (93) = happyShift action_18
action_0 (94) = happyShift action_19
action_0 (54) = happyGoto action_3
action_0 (55) = happyGoto action_4
action_0 (56) = happyGoto action_5
action_0 (61) = happyGoto action_6
action_0 (62) = happyGoto action_7
action_0 (63) = happyGoto action_8
action_0 (68) = happyGoto action_9
action_0 (69) = happyGoto action_10
action_0 (70) = happyGoto action_11
action_0 (74) = happyGoto action_12
action_0 _ = happyReduce_59

action_1 (117) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (167) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (88) = happyShift action_13
action_4 (89) = happyShift action_14
action_4 (90) = happyShift action_15
action_4 (91) = happyShift action_16
action_4 (92) = happyShift action_17
action_4 (93) = happyShift action_18
action_4 (94) = happyShift action_19
action_4 (55) = happyGoto action_4
action_4 (56) = happyGoto action_31
action_4 (61) = happyGoto action_6
action_4 (62) = happyGoto action_7
action_4 (63) = happyGoto action_8
action_4 (68) = happyGoto action_9
action_4 (69) = happyGoto action_10
action_4 (70) = happyGoto action_11
action_4 (74) = happyGoto action_12
action_4 _ = happyReduce_59

action_5 (160) = happyShift action_30
action_5 (47) = happyGoto action_28
action_5 (77) = happyGoto action_29
action_5 _ = happyReduce_94

action_6 _ = happyReduce_52

action_7 _ = happyReduce_55

action_8 _ = happyReduce_56

action_9 _ = happyReduce_53

action_10 _ = happyReduce_54

action_11 _ = happyReduce_57

action_12 _ = happyReduce_58

action_13 (100) = happyShift action_27
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (100) = happyShift action_26
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (100) = happyShift action_25
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (100) = happyShift action_24
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (166) = happyShift action_23
action_17 (53) = happyGoto action_22
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (100) = happyShift action_21
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (100) = happyShift action_20
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (114) = happyShift action_39
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (114) = happyShift action_38
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_68

action_23 _ = happyReduce_50

action_24 (114) = happyShift action_37
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (114) = happyShift action_36
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (114) = happyShift action_35
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (114) = happyShift action_34
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (95) = happyShift action_33
action_28 (78) = happyGoto action_32
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_51

action_30 _ = happyReduce_44

action_31 _ = happyReduce_60

action_32 (100) = happyShift action_57
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (115) = happyShift action_56
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (163) = happyShift action_43
action_34 (50) = happyGoto action_51
action_34 (64) = happyGoto action_52
action_34 (66) = happyGoto action_55
action_34 _ = happyReduce_73

action_35 (163) = happyShift action_43
action_35 (50) = happyGoto action_40
action_35 (57) = happyGoto action_41
action_35 (59) = happyGoto action_54
action_35 _ = happyReduce_63

action_36 (163) = happyShift action_43
action_36 (50) = happyGoto action_51
action_36 (64) = happyGoto action_52
action_36 (66) = happyGoto action_53
action_36 _ = happyReduce_73

action_37 (164) = happyShift action_47
action_37 (51) = happyGoto action_48
action_37 (75) = happyGoto action_49
action_37 (76) = happyGoto action_50
action_37 _ = happyReduce_89

action_38 (164) = happyShift action_47
action_38 (51) = happyGoto action_44
action_38 (71) = happyGoto action_45
action_38 (72) = happyGoto action_46
action_38 _ = happyReduce_81

action_39 (163) = happyShift action_43
action_39 (50) = happyGoto action_40
action_39 (57) = happyGoto action_41
action_39 (59) = happyGoto action_42
action_39 _ = happyReduce_63

action_40 (103) = happyShift action_75
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (102) = happyShift action_74
action_41 _ = happyReduce_64

action_42 (116) = happyShift action_73
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_47

action_44 (95) = happyShift action_72
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (116) = happyShift action_71
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (102) = happyShift action_70
action_46 _ = happyReduce_82

action_47 _ = happyReduce_48

action_48 (95) = happyShift action_69
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (116) = happyShift action_68
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (102) = happyShift action_67
action_50 _ = happyReduce_90

action_51 (103) = happyShift action_66
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (102) = happyShift action_65
action_52 _ = happyReduce_74

action_53 (116) = happyShift action_64
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (116) = happyShift action_63
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (116) = happyShift action_62
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (164) = happyShift action_47
action_56 (51) = happyGoto action_60
action_56 (73) = happyGoto action_61
action_56 _ = happyReduce_85

action_57 (114) = happyShift action_59
action_57 (79) = happyGoto action_58
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_93

action_59 (87) = happyShift action_130
action_59 (95) = happyShift action_131
action_59 (118) = happyShift action_132
action_59 (120) = happyShift action_133
action_59 (121) = happyShift action_134
action_59 (122) = happyShift action_135
action_59 (123) = happyShift action_136
action_59 (124) = happyShift action_137
action_59 (125) = happyShift action_138
action_59 (126) = happyShift action_139
action_59 (127) = happyShift action_140
action_59 (128) = happyShift action_141
action_59 (129) = happyShift action_142
action_59 (130) = happyShift action_143
action_59 (131) = happyShift action_144
action_59 (132) = happyShift action_145
action_59 (133) = happyShift action_146
action_59 (134) = happyShift action_147
action_59 (135) = happyShift action_148
action_59 (136) = happyShift action_149
action_59 (137) = happyShift action_150
action_59 (138) = happyShift action_151
action_59 (139) = happyShift action_152
action_59 (140) = happyShift action_153
action_59 (141) = happyShift action_154
action_59 (142) = happyShift action_155
action_59 (143) = happyShift action_156
action_59 (144) = happyShift action_157
action_59 (145) = happyShift action_158
action_59 (146) = happyShift action_159
action_59 (147) = happyShift action_160
action_59 (148) = happyShift action_161
action_59 (149) = happyShift action_162
action_59 (150) = happyShift action_163
action_59 (151) = happyShift action_164
action_59 (152) = happyShift action_165
action_59 (153) = happyShift action_166
action_59 (154) = happyShift action_167
action_59 (155) = happyShift action_168
action_59 (156) = happyShift action_169
action_59 (157) = happyShift action_170
action_59 (158) = happyShift action_171
action_59 (163) = happyShift action_43
action_59 (164) = happyShift action_47
action_59 (5) = happyGoto action_86
action_59 (7) = happyGoto action_87
action_59 (8) = happyGoto action_88
action_59 (9) = happyGoto action_89
action_59 (10) = happyGoto action_90
action_59 (11) = happyGoto action_91
action_59 (12) = happyGoto action_92
action_59 (13) = happyGoto action_93
action_59 (14) = happyGoto action_94
action_59 (15) = happyGoto action_95
action_59 (16) = happyGoto action_96
action_59 (17) = happyGoto action_97
action_59 (18) = happyGoto action_98
action_59 (19) = happyGoto action_99
action_59 (20) = happyGoto action_100
action_59 (21) = happyGoto action_101
action_59 (22) = happyGoto action_102
action_59 (23) = happyGoto action_103
action_59 (24) = happyGoto action_104
action_59 (25) = happyGoto action_105
action_59 (26) = happyGoto action_106
action_59 (27) = happyGoto action_107
action_59 (28) = happyGoto action_108
action_59 (29) = happyGoto action_109
action_59 (30) = happyGoto action_110
action_59 (31) = happyGoto action_111
action_59 (32) = happyGoto action_112
action_59 (33) = happyGoto action_113
action_59 (34) = happyGoto action_114
action_59 (35) = happyGoto action_115
action_59 (36) = happyGoto action_116
action_59 (37) = happyGoto action_117
action_59 (38) = happyGoto action_118
action_59 (39) = happyGoto action_119
action_59 (40) = happyGoto action_120
action_59 (41) = happyGoto action_121
action_59 (42) = happyGoto action_122
action_59 (43) = happyGoto action_123
action_59 (44) = happyGoto action_124
action_59 (45) = happyGoto action_125
action_59 (50) = happyGoto action_126
action_59 (51) = happyGoto action_127
action_59 (80) = happyGoto action_128
action_59 (81) = happyGoto action_129
action_59 _ = happyReduce_97

action_60 (97) = happyShift action_85
action_60 _ = happyReduce_86

action_61 (104) = happyShift action_84
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_69

action_63 _ = happyReduce_79

action_64 _ = happyReduce_70

action_65 (163) = happyShift action_43
action_65 (50) = happyGoto action_51
action_65 (64) = happyGoto action_52
action_65 (66) = happyGoto action_83
action_65 _ = happyReduce_73

action_66 (114) = happyShift action_82
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (164) = happyShift action_47
action_67 (51) = happyGoto action_48
action_67 (75) = happyGoto action_81
action_67 (76) = happyGoto action_50
action_67 _ = happyReduce_89

action_68 _ = happyReduce_88

action_69 (164) = happyShift action_47
action_69 (51) = happyGoto action_60
action_69 (73) = happyGoto action_80
action_69 _ = happyReduce_85

action_70 (164) = happyShift action_47
action_70 (51) = happyGoto action_44
action_70 (71) = happyGoto action_79
action_70 (72) = happyGoto action_46
action_70 _ = happyReduce_81

action_71 _ = happyReduce_80

action_72 (164) = happyShift action_47
action_72 (51) = happyGoto action_60
action_72 (73) = happyGoto action_78
action_72 _ = happyReduce_85

action_73 _ = happyReduce_78

action_74 (163) = happyShift action_43
action_74 (50) = happyGoto action_40
action_74 (57) = happyGoto action_41
action_74 (59) = happyGoto action_77
action_74 _ = happyReduce_63

action_75 (114) = happyShift action_76
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (163) = happyShift action_43
action_76 (50) = happyGoto action_211
action_76 (58) = happyGoto action_212
action_76 (60) = happyGoto action_213
action_76 _ = happyFail (happyExpListPerState 76)

action_77 _ = happyReduce_65

action_78 (115) = happyShift action_210
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_83

action_80 (96) = happyShift action_209
action_80 _ = happyFail (happyExpListPerState 80)

action_81 _ = happyReduce_91

action_82 (163) = happyShift action_43
action_82 (50) = happyGoto action_206
action_82 (65) = happyGoto action_207
action_82 (67) = happyGoto action_208
action_82 _ = happyFail (happyExpListPerState 82)

action_83 _ = happyReduce_75

action_84 (164) = happyShift action_47
action_84 (51) = happyGoto action_60
action_84 (73) = happyGoto action_205
action_84 _ = happyReduce_85

action_85 (164) = happyShift action_47
action_85 (51) = happyGoto action_60
action_85 (73) = happyGoto action_204
action_85 _ = happyReduce_85

action_86 _ = happyReduce_135

action_87 (164) = happyShift action_47
action_87 (51) = happyGoto action_203
action_87 _ = happyFail (happyExpListPerState 87)

action_88 _ = happyReduce_102

action_89 (164) = happyShift action_47
action_89 (51) = happyGoto action_202
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (165) = happyShift action_174
action_90 (52) = happyGoto action_201
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (162) = happyShift action_200
action_91 (49) = happyGoto action_199
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (161) = happyShift action_198
action_92 (48) = happyGoto action_197
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (118) = happyShift action_132
action_93 (5) = happyGoto action_196
action_93 _ = happyFail (happyExpListPerState 93)

action_94 _ = happyReduce_107

action_95 _ = happyReduce_108

action_96 _ = happyReduce_109

action_97 _ = happyReduce_110

action_98 _ = happyReduce_111

action_99 _ = happyReduce_113

action_100 _ = happyReduce_114

action_101 _ = happyReduce_115

action_102 _ = happyReduce_116

action_103 _ = happyReduce_117

action_104 _ = happyReduce_118

action_105 _ = happyReduce_119

action_106 (117) = happyShift action_2
action_106 (4) = happyGoto action_195
action_106 _ = happyFail (happyExpListPerState 106)

action_107 _ = happyReduce_121

action_108 _ = happyReduce_122

action_109 _ = happyReduce_123

action_110 _ = happyReduce_124

action_111 _ = happyReduce_125

action_112 (164) = happyShift action_47
action_112 (51) = happyGoto action_194
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (164) = happyShift action_47
action_113 (51) = happyGoto action_193
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (110) = happyShift action_192
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (164) = happyShift action_47
action_115 (51) = happyGoto action_191
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (164) = happyShift action_47
action_116 (51) = happyGoto action_190
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (163) = happyShift action_43
action_117 (50) = happyGoto action_189
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (164) = happyShift action_47
action_118 (51) = happyGoto action_188
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (164) = happyShift action_47
action_119 (51) = happyGoto action_187
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (164) = happyShift action_47
action_120 (51) = happyGoto action_186
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (164) = happyShift action_47
action_121 (51) = happyGoto action_60
action_121 (73) = happyGoto action_185
action_121 _ = happyReduce_85

action_122 (164) = happyShift action_47
action_122 (51) = happyGoto action_184
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (114) = happyShift action_183
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (164) = happyShift action_47
action_124 (51) = happyGoto action_182
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (164) = happyShift action_47
action_125 (51) = happyGoto action_181
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (99) = happyShift action_180
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (101) = happyShift action_178
action_127 (159) = happyShift action_179
action_127 (46) = happyGoto action_177
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (116) = happyShift action_176
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (102) = happyShift action_175
action_129 _ = happyReduce_98

action_130 (165) = happyShift action_174
action_130 (52) = happyGoto action_173
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (164) = happyShift action_47
action_131 (51) = happyGoto action_60
action_131 (73) = happyGoto action_172
action_131 _ = happyReduce_85

action_132 _ = happyReduce_2

action_133 _ = happyReduce_4

action_134 _ = happyReduce_5

action_135 _ = happyReduce_6

action_136 _ = happyReduce_7

action_137 _ = happyReduce_8

action_138 _ = happyReduce_9

action_139 _ = happyReduce_10

action_140 _ = happyReduce_11

action_141 _ = happyReduce_12

action_142 _ = happyReduce_13

action_143 _ = happyReduce_14

action_144 _ = happyReduce_15

action_145 _ = happyReduce_16

action_146 _ = happyReduce_17

action_147 _ = happyReduce_18

action_148 _ = happyReduce_19

action_149 _ = happyReduce_20

action_150 _ = happyReduce_21

action_151 _ = happyReduce_22

action_152 _ = happyReduce_23

action_153 _ = happyReduce_24

action_154 _ = happyReduce_25

action_155 _ = happyReduce_26

action_156 _ = happyReduce_27

action_157 _ = happyReduce_28

action_158 _ = happyReduce_29

action_159 _ = happyReduce_30

action_160 _ = happyReduce_31

action_161 _ = happyReduce_32

action_162 _ = happyReduce_33

action_163 _ = happyReduce_34

action_164 _ = happyReduce_35

action_165 _ = happyReduce_36

action_166 _ = happyReduce_37

action_167 _ = happyReduce_38

action_168 _ = happyReduce_39

action_169 _ = happyReduce_40

action_170 _ = happyReduce_41

action_171 _ = happyReduce_42

action_172 (96) = happyShift action_242
action_172 _ = happyFail (happyExpListPerState 172)

action_173 (95) = happyShift action_241
action_173 _ = happyFail (happyExpListPerState 173)

action_174 _ = happyReduce_49

action_175 (87) = happyShift action_130
action_175 (95) = happyShift action_131
action_175 (118) = happyShift action_132
action_175 (120) = happyShift action_133
action_175 (121) = happyShift action_134
action_175 (122) = happyShift action_135
action_175 (123) = happyShift action_136
action_175 (124) = happyShift action_137
action_175 (125) = happyShift action_138
action_175 (126) = happyShift action_139
action_175 (127) = happyShift action_140
action_175 (128) = happyShift action_141
action_175 (129) = happyShift action_142
action_175 (130) = happyShift action_143
action_175 (131) = happyShift action_144
action_175 (132) = happyShift action_145
action_175 (133) = happyShift action_146
action_175 (134) = happyShift action_147
action_175 (135) = happyShift action_148
action_175 (136) = happyShift action_149
action_175 (137) = happyShift action_150
action_175 (138) = happyShift action_151
action_175 (139) = happyShift action_152
action_175 (140) = happyShift action_153
action_175 (141) = happyShift action_154
action_175 (142) = happyShift action_155
action_175 (143) = happyShift action_156
action_175 (144) = happyShift action_157
action_175 (145) = happyShift action_158
action_175 (146) = happyShift action_159
action_175 (147) = happyShift action_160
action_175 (148) = happyShift action_161
action_175 (149) = happyShift action_162
action_175 (150) = happyShift action_163
action_175 (151) = happyShift action_164
action_175 (152) = happyShift action_165
action_175 (153) = happyShift action_166
action_175 (154) = happyShift action_167
action_175 (155) = happyShift action_168
action_175 (156) = happyShift action_169
action_175 (157) = happyShift action_170
action_175 (158) = happyShift action_171
action_175 (163) = happyShift action_43
action_175 (164) = happyShift action_47
action_175 (5) = happyGoto action_86
action_175 (7) = happyGoto action_87
action_175 (8) = happyGoto action_88
action_175 (9) = happyGoto action_89
action_175 (10) = happyGoto action_90
action_175 (11) = happyGoto action_91
action_175 (12) = happyGoto action_92
action_175 (13) = happyGoto action_93
action_175 (14) = happyGoto action_94
action_175 (15) = happyGoto action_95
action_175 (16) = happyGoto action_96
action_175 (17) = happyGoto action_97
action_175 (18) = happyGoto action_98
action_175 (19) = happyGoto action_99
action_175 (20) = happyGoto action_100
action_175 (21) = happyGoto action_101
action_175 (22) = happyGoto action_102
action_175 (23) = happyGoto action_103
action_175 (24) = happyGoto action_104
action_175 (25) = happyGoto action_105
action_175 (26) = happyGoto action_106
action_175 (27) = happyGoto action_107
action_175 (28) = happyGoto action_108
action_175 (29) = happyGoto action_109
action_175 (30) = happyGoto action_110
action_175 (31) = happyGoto action_111
action_175 (32) = happyGoto action_112
action_175 (33) = happyGoto action_113
action_175 (34) = happyGoto action_114
action_175 (35) = happyGoto action_115
action_175 (36) = happyGoto action_116
action_175 (37) = happyGoto action_117
action_175 (38) = happyGoto action_118
action_175 (39) = happyGoto action_119
action_175 (40) = happyGoto action_120
action_175 (41) = happyGoto action_121
action_175 (42) = happyGoto action_122
action_175 (43) = happyGoto action_123
action_175 (44) = happyGoto action_124
action_175 (45) = happyGoto action_125
action_175 (50) = happyGoto action_126
action_175 (51) = happyGoto action_127
action_175 (80) = happyGoto action_240
action_175 (81) = happyGoto action_129
action_175 _ = happyReduce_97

action_176 _ = happyReduce_96

action_177 (164) = happyShift action_47
action_177 (51) = happyGoto action_239
action_177 _ = happyFail (happyExpListPerState 177)

action_178 (87) = happyShift action_130
action_178 (95) = happyShift action_131
action_178 (118) = happyShift action_132
action_178 (120) = happyShift action_133
action_178 (121) = happyShift action_134
action_178 (122) = happyShift action_135
action_178 (123) = happyShift action_136
action_178 (124) = happyShift action_137
action_178 (125) = happyShift action_138
action_178 (126) = happyShift action_139
action_178 (127) = happyShift action_140
action_178 (128) = happyShift action_141
action_178 (129) = happyShift action_142
action_178 (130) = happyShift action_143
action_178 (131) = happyShift action_144
action_178 (132) = happyShift action_145
action_178 (133) = happyShift action_146
action_178 (134) = happyShift action_147
action_178 (135) = happyShift action_148
action_178 (136) = happyShift action_149
action_178 (137) = happyShift action_150
action_178 (138) = happyShift action_151
action_178 (139) = happyShift action_152
action_178 (140) = happyShift action_153
action_178 (141) = happyShift action_154
action_178 (142) = happyShift action_155
action_178 (143) = happyShift action_156
action_178 (144) = happyShift action_157
action_178 (145) = happyShift action_158
action_178 (146) = happyShift action_159
action_178 (147) = happyShift action_160
action_178 (148) = happyShift action_161
action_178 (149) = happyShift action_162
action_178 (150) = happyShift action_163
action_178 (151) = happyShift action_164
action_178 (152) = happyShift action_165
action_178 (153) = happyShift action_166
action_178 (154) = happyShift action_167
action_178 (155) = happyShift action_168
action_178 (156) = happyShift action_169
action_178 (157) = happyShift action_170
action_178 (158) = happyShift action_171
action_178 (163) = happyShift action_43
action_178 (164) = happyShift action_47
action_178 (5) = happyGoto action_86
action_178 (7) = happyGoto action_87
action_178 (8) = happyGoto action_88
action_178 (9) = happyGoto action_89
action_178 (10) = happyGoto action_90
action_178 (11) = happyGoto action_91
action_178 (12) = happyGoto action_92
action_178 (13) = happyGoto action_93
action_178 (14) = happyGoto action_94
action_178 (15) = happyGoto action_95
action_178 (16) = happyGoto action_96
action_178 (17) = happyGoto action_97
action_178 (18) = happyGoto action_98
action_178 (19) = happyGoto action_99
action_178 (20) = happyGoto action_100
action_178 (21) = happyGoto action_101
action_178 (22) = happyGoto action_102
action_178 (23) = happyGoto action_103
action_178 (24) = happyGoto action_104
action_178 (25) = happyGoto action_105
action_178 (26) = happyGoto action_106
action_178 (27) = happyGoto action_107
action_178 (28) = happyGoto action_108
action_178 (29) = happyGoto action_109
action_178 (30) = happyGoto action_110
action_178 (31) = happyGoto action_111
action_178 (32) = happyGoto action_112
action_178 (33) = happyGoto action_113
action_178 (34) = happyGoto action_114
action_178 (35) = happyGoto action_115
action_178 (36) = happyGoto action_116
action_178 (37) = happyGoto action_117
action_178 (38) = happyGoto action_118
action_178 (39) = happyGoto action_119
action_178 (40) = happyGoto action_120
action_178 (41) = happyGoto action_121
action_178 (42) = happyGoto action_122
action_178 (43) = happyGoto action_123
action_178 (44) = happyGoto action_124
action_178 (45) = happyGoto action_125
action_178 (50) = happyGoto action_126
action_178 (51) = happyGoto action_127
action_178 (81) = happyGoto action_238
action_178 _ = happyFail (happyExpListPerState 178)

action_179 _ = happyReduce_43

action_180 (163) = happyShift action_43
action_180 (50) = happyGoto action_237
action_180 _ = happyFail (happyExpListPerState 180)

action_181 _ = happyReduce_147

action_182 _ = happyReduce_146

action_183 (164) = happyShift action_47
action_183 (51) = happyGoto action_234
action_183 (85) = happyGoto action_235
action_183 (86) = happyGoto action_236
action_183 _ = happyReduce_157

action_184 (95) = happyShift action_233
action_184 _ = happyFail (happyExpListPerState 184)

action_185 (107) = happyShift action_232
action_185 _ = happyFail (happyExpListPerState 185)

action_186 (107) = happyShift action_231
action_186 _ = happyFail (happyExpListPerState 186)

action_187 (109) = happyShift action_230
action_187 _ = happyFail (happyExpListPerState 187)

action_188 (110) = happyShift action_229
action_188 _ = happyFail (happyExpListPerState 188)

action_189 (99) = happyShift action_228
action_189 _ = happyFail (happyExpListPerState 189)

action_190 (111) = happyShift action_227
action_190 _ = happyFail (happyExpListPerState 190)

action_191 (111) = happyShift action_226
action_191 _ = happyFail (happyExpListPerState 191)

action_192 (114) = happyShift action_225
action_192 _ = happyFail (happyExpListPerState 192)

action_193 (112) = happyShift action_224
action_193 _ = happyFail (happyExpListPerState 193)

action_194 (110) = happyShift action_223
action_194 _ = happyFail (happyExpListPerState 194)

action_195 _ = happyReduce_120

action_196 _ = happyReduce_106

action_197 _ = happyReduce_112

action_198 _ = happyReduce_45

action_199 _ = happyReduce_105

action_200 _ = happyReduce_46

action_201 _ = happyReduce_104

action_202 (95) = happyShift action_222
action_202 _ = happyFail (happyExpListPerState 202)

action_203 _ = happyReduce_101

action_204 _ = happyReduce_87

action_205 (96) = happyShift action_221
action_205 _ = happyFail (happyExpListPerState 205)

action_206 (165) = happyShift action_174
action_206 (52) = happyGoto action_220
action_206 _ = happyFail (happyExpListPerState 206)

action_207 (102) = happyShift action_219
action_207 _ = happyReduce_76

action_208 (116) = happyShift action_218
action_208 _ = happyFail (happyExpListPerState 208)

action_209 (103) = happyShift action_217
action_209 _ = happyFail (happyExpListPerState 209)

action_210 (164) = happyShift action_47
action_210 (51) = happyGoto action_60
action_210 (73) = happyGoto action_216
action_210 _ = happyReduce_85

action_211 _ = happyReduce_62

action_212 (102) = happyShift action_215
action_212 _ = happyReduce_66

action_213 (116) = happyShift action_214
action_213 _ = happyFail (happyExpListPerState 213)

action_214 _ = happyReduce_61

action_215 (163) = happyShift action_43
action_215 (50) = happyGoto action_211
action_215 (58) = happyGoto action_212
action_215 (60) = happyGoto action_266
action_215 _ = happyFail (happyExpListPerState 215)

action_216 (104) = happyShift action_265
action_216 _ = happyFail (happyExpListPerState 216)

action_217 (114) = happyShift action_59
action_217 (79) = happyGoto action_264
action_217 _ = happyFail (happyExpListPerState 217)

action_218 _ = happyReduce_71

action_219 (163) = happyShift action_43
action_219 (50) = happyGoto action_206
action_219 (65) = happyGoto action_207
action_219 (67) = happyGoto action_263
action_219 _ = happyFail (happyExpListPerState 219)

action_220 _ = happyReduce_72

action_221 _ = happyReduce_95

action_222 (164) = happyShift action_47
action_222 (51) = happyGoto action_60
action_222 (73) = happyGoto action_262
action_222 _ = happyReduce_85

action_223 (114) = happyShift action_261
action_223 _ = happyFail (happyExpListPerState 223)

action_224 (114) = happyShift action_59
action_224 (79) = happyGoto action_260
action_224 _ = happyFail (happyExpListPerState 224)

action_225 (163) = happyShift action_43
action_225 (50) = happyGoto action_257
action_225 (82) = happyGoto action_258
action_225 (84) = happyGoto action_259
action_225 _ = happyReduce_153

action_226 (164) = happyShift action_47
action_226 (51) = happyGoto action_256
action_226 _ = happyFail (happyExpListPerState 226)

action_227 (164) = happyShift action_47
action_227 (51) = happyGoto action_255
action_227 _ = happyFail (happyExpListPerState 227)

action_228 (163) = happyShift action_43
action_228 (50) = happyGoto action_254
action_228 _ = happyFail (happyExpListPerState 228)

action_229 (114) = happyShift action_253
action_229 _ = happyFail (happyExpListPerState 229)

action_230 (164) = happyShift action_47
action_230 (51) = happyGoto action_252
action_230 _ = happyFail (happyExpListPerState 230)

action_231 (114) = happyShift action_251
action_231 _ = happyFail (happyExpListPerState 231)

action_232 (114) = happyShift action_250
action_232 _ = happyFail (happyExpListPerState 232)

action_233 (164) = happyShift action_47
action_233 (51) = happyGoto action_60
action_233 (73) = happyGoto action_249
action_233 _ = happyReduce_85

action_234 (98) = happyShift action_248
action_234 _ = happyFail (happyExpListPerState 234)

action_235 (102) = happyShift action_247
action_235 _ = happyReduce_158

action_236 (116) = happyShift action_246
action_236 _ = happyFail (happyExpListPerState 236)

action_237 (95) = happyShift action_245
action_237 (164) = happyShift action_47
action_237 (51) = happyGoto action_244
action_237 _ = happyReduce_126

action_238 _ = happyReduce_100

action_239 _ = happyReduce_144

action_240 _ = happyReduce_99

action_241 (164) = happyShift action_47
action_241 (51) = happyGoto action_243
action_241 _ = happyFail (happyExpListPerState 241)

action_242 _ = happyReduce_133

action_243 (96) = happyShift action_283
action_243 _ = happyFail (happyExpListPerState 243)

action_244 _ = happyReduce_131

action_245 (164) = happyShift action_47
action_245 (51) = happyGoto action_60
action_245 (73) = happyGoto action_282
action_245 _ = happyReduce_85

action_246 _ = happyReduce_145

action_247 (164) = happyShift action_47
action_247 (51) = happyGoto action_234
action_247 (85) = happyGoto action_235
action_247 (86) = happyGoto action_281
action_247 _ = happyReduce_157

action_248 (114) = happyShift action_59
action_248 (79) = happyGoto action_280
action_248 _ = happyFail (happyExpListPerState 248)

action_249 (115) = happyShift action_279
action_249 _ = happyFail (happyExpListPerState 249)

action_250 (113) = happyShift action_278
action_250 _ = happyFail (happyExpListPerState 250)

action_251 (164) = happyShift action_47
action_251 (51) = happyGoto action_277
action_251 _ = happyFail (happyExpListPerState 251)

action_252 (164) = happyShift action_47
action_252 (51) = happyGoto action_276
action_252 _ = happyFail (happyExpListPerState 252)

action_253 (163) = happyShift action_43
action_253 (50) = happyGoto action_257
action_253 (82) = happyGoto action_258
action_253 (84) = happyGoto action_275
action_253 _ = happyReduce_153

action_254 (111) = happyShift action_274
action_254 _ = happyFail (happyExpListPerState 254)

action_255 _ = happyReduce_137

action_256 _ = happyReduce_136

action_257 (99) = happyShift action_273
action_257 _ = happyFail (happyExpListPerState 257)

action_258 (102) = happyShift action_272
action_258 _ = happyReduce_154

action_259 (116) = happyShift action_271
action_259 _ = happyFail (happyExpListPerState 259)

action_260 (108) = happyShift action_270
action_260 _ = happyFail (happyExpListPerState 260)

action_261 (163) = happyShift action_43
action_261 (50) = happyGoto action_257
action_261 (82) = happyGoto action_258
action_261 (84) = happyGoto action_269
action_261 _ = happyReduce_153

action_262 (96) = happyShift action_268
action_262 _ = happyFail (happyExpListPerState 262)

action_263 _ = happyReduce_77

action_264 _ = happyReduce_92

action_265 (164) = happyShift action_47
action_265 (51) = happyGoto action_60
action_265 (73) = happyGoto action_267
action_265 _ = happyReduce_85

action_266 _ = happyReduce_67

action_267 (96) = happyShift action_294
action_267 _ = happyFail (happyExpListPerState 267)

action_268 _ = happyReduce_103

action_269 (116) = happyShift action_293
action_269 _ = happyFail (happyExpListPerState 269)

action_270 (114) = happyShift action_59
action_270 (79) = happyGoto action_292
action_270 _ = happyFail (happyExpListPerState 270)

action_271 _ = happyReduce_130

action_272 (163) = happyShift action_43
action_272 (50) = happyGoto action_257
action_272 (82) = happyGoto action_258
action_272 (84) = happyGoto action_291
action_272 _ = happyReduce_153

action_273 (163) = happyShift action_43
action_273 (50) = happyGoto action_290
action_273 _ = happyFail (happyExpListPerState 273)

action_274 (164) = happyShift action_47
action_274 (51) = happyGoto action_289
action_274 _ = happyFail (happyExpListPerState 274)

action_275 (116) = happyShift action_288
action_275 _ = happyFail (happyExpListPerState 275)

action_276 _ = happyReduce_140

action_277 (113) = happyShift action_287
action_277 _ = happyFail (happyExpListPerState 277)

action_278 (105) = happyShift action_286
action_278 _ = happyFail (happyExpListPerState 278)

action_279 (164) = happyShift action_47
action_279 (51) = happyGoto action_60
action_279 (73) = happyGoto action_285
action_279 _ = happyReduce_85

action_280 _ = happyReduce_156

action_281 _ = happyReduce_159

action_282 (96) = happyShift action_284
action_282 _ = happyFail (happyExpListPerState 282)

action_283 _ = happyReduce_134

action_284 (164) = happyShift action_47
action_284 (51) = happyGoto action_301
action_284 _ = happyReduce_127

action_285 (104) = happyShift action_300
action_285 _ = happyFail (happyExpListPerState 285)

action_286 (164) = happyShift action_47
action_286 (51) = happyGoto action_60
action_286 (73) = happyGoto action_299
action_286 _ = happyReduce_85

action_287 (164) = happyShift action_47
action_287 (51) = happyGoto action_60
action_287 (73) = happyGoto action_298
action_287 _ = happyReduce_85

action_288 _ = happyReduce_139

action_289 _ = happyReduce_138

action_290 (95) = happyShift action_296
action_290 (100) = happyShift action_297
action_290 _ = happyFail (happyExpListPerState 290)

action_291 _ = happyReduce_155

action_292 _ = happyReduce_129

action_293 _ = happyReduce_128

action_294 (103) = happyShift action_295
action_294 _ = happyFail (happyExpListPerState 294)

action_295 (114) = happyShift action_59
action_295 (79) = happyGoto action_307
action_295 _ = happyFail (happyExpListPerState 295)

action_296 (164) = happyShift action_47
action_296 (51) = happyGoto action_60
action_296 (73) = happyGoto action_306
action_296 _ = happyReduce_85

action_297 (114) = happyShift action_59
action_297 (79) = happyGoto action_305
action_297 _ = happyFail (happyExpListPerState 297)

action_298 (100) = happyShift action_304
action_298 _ = happyFail (happyExpListPerState 298)

action_299 (106) = happyShift action_303
action_299 _ = happyFail (happyExpListPerState 299)

action_300 (164) = happyShift action_47
action_300 (51) = happyGoto action_60
action_300 (73) = happyGoto action_302
action_300 _ = happyReduce_85

action_301 _ = happyReduce_132

action_302 (96) = happyShift action_311
action_302 _ = happyFail (happyExpListPerState 302)

action_303 (100) = happyShift action_310
action_303 _ = happyFail (happyExpListPerState 303)

action_304 (114) = happyShift action_59
action_304 (79) = happyGoto action_309
action_304 _ = happyFail (happyExpListPerState 304)

action_305 _ = happyReduce_148

action_306 (96) = happyShift action_308
action_306 _ = happyFail (happyExpListPerState 306)

action_307 _ = happyReduce_84

action_308 (100) = happyShift action_314
action_308 _ = happyFail (happyExpListPerState 308)

action_309 (102) = happyShift action_313
action_309 _ = happyFail (happyExpListPerState 309)

action_310 (114) = happyShift action_59
action_310 (79) = happyGoto action_312
action_310 _ = happyFail (happyExpListPerState 310)

action_311 _ = happyReduce_143

action_312 (102) = happyShift action_317
action_312 _ = happyFail (happyExpListPerState 312)

action_313 (164) = happyShift action_47
action_313 (51) = happyGoto action_316
action_313 _ = happyFail (happyExpListPerState 313)

action_314 (114) = happyShift action_59
action_314 (79) = happyGoto action_315
action_314 _ = happyFail (happyExpListPerState 314)

action_315 _ = happyReduce_149

action_316 (113) = happyShift action_319
action_316 _ = happyFail (happyExpListPerState 316)

action_317 (113) = happyShift action_318
action_317 _ = happyFail (happyExpListPerState 317)

action_318 (105) = happyShift action_321
action_318 _ = happyFail (happyExpListPerState 318)

action_319 (164) = happyShift action_47
action_319 (51) = happyGoto action_60
action_319 (73) = happyGoto action_320
action_319 _ = happyReduce_85

action_320 (100) = happyShift action_323
action_320 _ = happyFail (happyExpListPerState 320)

action_321 (164) = happyShift action_47
action_321 (51) = happyGoto action_60
action_321 (73) = happyGoto action_322
action_321 _ = happyReduce_85

action_322 (106) = happyShift action_325
action_322 _ = happyFail (happyExpListPerState 322)

action_323 (114) = happyShift action_59
action_323 (79) = happyGoto action_324
action_323 _ = happyFail (happyExpListPerState 323)

action_324 (116) = happyShift action_327
action_324 _ = happyFail (happyExpListPerState 324)

action_325 (100) = happyShift action_326
action_325 _ = happyFail (happyExpListPerState 325)

action_326 (114) = happyShift action_59
action_326 (79) = happyGoto action_328
action_326 _ = happyFail (happyExpListPerState 326)

action_327 _ = happyReduce_141

action_328 (116) = happyShift action_329
action_328 _ = happyFail (happyExpListPerState 328)

action_329 _ = happyReduce_142

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
		 (MplAsmLanguage.AbsMPLASM.LeqC (mkPosToken happy_var_1)
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  23 happyReduction_20
happyReduction_20 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn23
		 (MplAsmLanguage.AbsMPLASM.EqC (mkPosToken happy_var_1)
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  24 happyReduction_21
happyReduction_21 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn24
		 (MplAsmLanguage.AbsMPLASM.Leqs (mkPosToken happy_var_1)
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  25 happyReduction_22
happyReduction_22 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn25
		 (MplAsmLanguage.AbsMPLASM.Eqs (mkPosToken happy_var_1)
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  26 happyReduction_23
happyReduction_23 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn26
		 (MplAsmLanguage.AbsMPLASM.ConcatS (mkPosToken happy_var_1)
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  27 happyReduction_24
happyReduction_24 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (MplAsmLanguage.AbsMPLASM.Add (mkPosToken happy_var_1)
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  28 happyReduction_25
happyReduction_25 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn28
		 (MplAsmLanguage.AbsMPLASM.Subtract (mkPosToken happy_var_1)
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  29 happyReduction_26
happyReduction_26 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (MplAsmLanguage.AbsMPLASM.Mul (mkPosToken happy_var_1)
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  30 happyReduction_27
happyReduction_27 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn30
		 (MplAsmLanguage.AbsMPLASM.Quot (mkPosToken happy_var_1)
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  31 happyReduction_28
happyReduction_28 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn31
		 (MplAsmLanguage.AbsMPLASM.Rem (mkPosToken happy_var_1)
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  32 happyReduction_29
happyReduction_29 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn32
		 (MplAsmLanguage.AbsMPLASM.Case (mkPosToken happy_var_1)
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  33 happyReduction_30
happyReduction_30 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn33
		 (MplAsmLanguage.AbsMPLASM.If (mkPosToken happy_var_1)
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  34 happyReduction_31
happyReduction_31 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn34
		 (MplAsmLanguage.AbsMPLASM.Rec (mkPosToken happy_var_1)
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  35 happyReduction_32
happyReduction_32 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn35
		 (MplAsmLanguage.AbsMPLASM.Get (mkPosToken happy_var_1)
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  36 happyReduction_33
happyReduction_33 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 (MplAsmLanguage.AbsMPLASM.Put (mkPosToken happy_var_1)
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  37 happyReduction_34
happyReduction_34 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn37
		 (MplAsmLanguage.AbsMPLASM.Hput (mkPosToken happy_var_1)
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  38 happyReduction_35
happyReduction_35 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn38
		 (MplAsmLanguage.AbsMPLASM.Hcase (mkPosToken happy_var_1)
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  39 happyReduction_36
happyReduction_36 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn39
		 (MplAsmLanguage.AbsMPLASM.Split (mkPosToken happy_var_1)
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  40 happyReduction_37
happyReduction_37 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn40
		 (MplAsmLanguage.AbsMPLASM.Fork (mkPosToken happy_var_1)
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  41 happyReduction_38
happyReduction_38 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn41
		 (MplAsmLanguage.AbsMPLASM.Plug (mkPosToken happy_var_1)
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  42 happyReduction_39
happyReduction_39 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn42
		 (MplAsmLanguage.AbsMPLASM.Run (mkPosToken happy_var_1)
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  43 happyReduction_40
happyReduction_40 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn43
		 (MplAsmLanguage.AbsMPLASM.Race (mkPosToken happy_var_1)
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  44 happyReduction_41
happyReduction_41 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn44
		 (MplAsmLanguage.AbsMPLASM.Close (mkPosToken happy_var_1)
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  45 happyReduction_42
happyReduction_42 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn45
		 (MplAsmLanguage.AbsMPLASM.Halt (mkPosToken happy_var_1)
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  46 happyReduction_43
happyReduction_43 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn46
		 (MplAsmLanguage.AbsMPLASM.Ch_Id (mkPosToken happy_var_1)
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  47 happyReduction_44
happyReduction_44 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn47
		 (MplAsmLanguage.AbsMPLASM.Main_run (mkPosToken happy_var_1)
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  48 happyReduction_45
happyReduction_45 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn48
		 (MplAsmLanguage.AbsMPLASM.BBool (mkPosToken happy_var_1)
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  49 happyReduction_46
happyReduction_46 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn49
		 (MplAsmLanguage.AbsMPLASM.Character (mkPosToken happy_var_1)
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  50 happyReduction_47
happyReduction_47 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn50
		 (MplAsmLanguage.AbsMPLASM.UIdent (mkPosToken happy_var_1)
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  51 happyReduction_48
happyReduction_48 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn51
		 (MplAsmLanguage.AbsMPLASM.PIdent (mkPosToken happy_var_1)
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  52 happyReduction_49
happyReduction_49 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn52
		 (MplAsmLanguage.AbsMPLASM.PInteger (mkPosToken happy_var_1)
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  53 happyReduction_50
happyReduction_50 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn53
		 (MplAsmLanguage.AbsMPLASM.IIdent (mkPosToken happy_var_1)
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_2  54 happyReduction_51
happyReduction_51 (HappyAbsSyn77  happy_var_2)
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn54
		 (MplAsmLanguage.AbsMPLASM.AMPLCODE happy_var_1 happy_var_2
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  55 happyReduction_52
happyReduction_52 (HappyAbsSyn61  happy_var_1)
	 =  HappyAbsSyn55
		 (MplAsmLanguage.AbsMPLASM.IMPORT_CONSTRUCT happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  55 happyReduction_53
happyReduction_53 (HappyAbsSyn68  happy_var_1)
	 =  HappyAbsSyn55
		 (MplAsmLanguage.AbsMPLASM.PROTOCOL_CONSTRUCT happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  55 happyReduction_54
happyReduction_54 (HappyAbsSyn69  happy_var_1)
	 =  HappyAbsSyn55
		 (MplAsmLanguage.AbsMPLASM.COPROTOCOL_CONSTRUCT happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  55 happyReduction_55
happyReduction_55 (HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn55
		 (MplAsmLanguage.AbsMPLASM.CONSTRUCTOR_CONSTRUCT happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  55 happyReduction_56
happyReduction_56 (HappyAbsSyn63  happy_var_1)
	 =  HappyAbsSyn55
		 (MplAsmLanguage.AbsMPLASM.DESTRUCTOR_CONSTRUCT happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  55 happyReduction_57
happyReduction_57 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn55
		 (MplAsmLanguage.AbsMPLASM.PROCESSES_CONSTRUCT happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  55 happyReduction_58
happyReduction_58 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn55
		 (MplAsmLanguage.AbsMPLASM.FUNCTIONS_CONSTRUCT happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_0  56 happyReduction_59
happyReduction_59  =  HappyAbsSyn56
		 ([]
	)

happyReduce_60 = happySpecReduce_2  56 happyReduction_60
happyReduction_60 (HappyAbsSyn56  happy_var_2)
	(HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn56
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_60 _ _  = notHappyAtAll 

happyReduce_61 = happyReduce 5 57 happyReduction_61
happyReduction_61 (_ `HappyStk`
	(HappyAbsSyn60  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn57
		 (MplAsmLanguage.AbsMPLASM.PROTOCOL_COPROTOCOL_SPEC happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_62 = happySpecReduce_1  58 happyReduction_62
happyReduction_62 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn58
		 (MplAsmLanguage.AbsMPLASM.HANDLE_NAME happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_0  59 happyReduction_63
happyReduction_63  =  HappyAbsSyn59
		 ([]
	)

happyReduce_64 = happySpecReduce_1  59 happyReduction_64
happyReduction_64 (HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn59
		 ((:[]) happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  59 happyReduction_65
happyReduction_65 (HappyAbsSyn59  happy_var_3)
	_
	(HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn59
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  60 happyReduction_66
happyReduction_66 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn60
		 ((:[]) happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  60 happyReduction_67
happyReduction_67 (HappyAbsSyn60  happy_var_3)
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn60
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_2  61 happyReduction_68
happyReduction_68 (HappyAbsSyn53  happy_var_2)
	_
	 =  HappyAbsSyn61
		 (MplAsmLanguage.AbsMPLASM.IMPORT happy_var_2
	)
happyReduction_68 _ _  = notHappyAtAll 

happyReduce_69 = happyReduce 5 62 happyReduction_69
happyReduction_69 (_ `HappyStk`
	(HappyAbsSyn66  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn62
		 (MplAsmLanguage.AbsMPLASM.CONSTRUCTORS happy_var_4
	) `HappyStk` happyRest

happyReduce_70 = happyReduce 5 63 happyReduction_70
happyReduction_70 (_ `HappyStk`
	(HappyAbsSyn66  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn63
		 (MplAsmLanguage.AbsMPLASM.DESTRUCTORS happy_var_4
	) `HappyStk` happyRest

happyReduce_71 = happyReduce 5 64 happyReduction_71
happyReduction_71 (_ `HappyStk`
	(HappyAbsSyn67  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn64
		 (MplAsmLanguage.AbsMPLASM.STRUCT_SPEC happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_72 = happySpecReduce_2  65 happyReduction_72
happyReduction_72 (HappyAbsSyn52  happy_var_2)
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn65
		 (MplAsmLanguage.AbsMPLASM.STRUCT happy_var_1 happy_var_2
	)
happyReduction_72 _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_0  66 happyReduction_73
happyReduction_73  =  HappyAbsSyn66
		 ([]
	)

happyReduce_74 = happySpecReduce_1  66 happyReduction_74
happyReduction_74 (HappyAbsSyn64  happy_var_1)
	 =  HappyAbsSyn66
		 ((:[]) happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_3  66 happyReduction_75
happyReduction_75 (HappyAbsSyn66  happy_var_3)
	_
	(HappyAbsSyn64  happy_var_1)
	 =  HappyAbsSyn66
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_75 _ _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  67 happyReduction_76
happyReduction_76 (HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn67
		 ((:[]) happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  67 happyReduction_77
happyReduction_77 (HappyAbsSyn67  happy_var_3)
	_
	(HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn67
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happyReduce 5 68 happyReduction_78
happyReduction_78 (_ `HappyStk`
	(HappyAbsSyn59  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn68
		 (MplAsmLanguage.AbsMPLASM.PROTOCOLS happy_var_4
	) `HappyStk` happyRest

happyReduce_79 = happyReduce 5 69 happyReduction_79
happyReduction_79 (_ `HappyStk`
	(HappyAbsSyn59  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn69
		 (MplAsmLanguage.AbsMPLASM.COPROTOCOLS happy_var_4
	) `HappyStk` happyRest

happyReduce_80 = happyReduce 5 70 happyReduction_80
happyReduction_80 (_ `HappyStk`
	(HappyAbsSyn71  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn70
		 (MplAsmLanguage.AbsMPLASM.PROCESSES happy_var_4
	) `HappyStk` happyRest

happyReduce_81 = happySpecReduce_0  71 happyReduction_81
happyReduction_81  =  HappyAbsSyn71
		 ([]
	)

happyReduce_82 = happySpecReduce_1  71 happyReduction_82
happyReduction_82 (HappyAbsSyn72  happy_var_1)
	 =  HappyAbsSyn71
		 ((:[]) happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_3  71 happyReduction_83
happyReduction_83 (HappyAbsSyn71  happy_var_3)
	_
	(HappyAbsSyn72  happy_var_1)
	 =  HappyAbsSyn71
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_83 _ _ _  = notHappyAtAll 

happyReduce_84 = happyReduce 10 72 happyReduction_84
happyReduction_84 ((HappyAbsSyn79  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn73  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn73  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn73  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn72
		 (MplAsmLanguage.AbsMPLASM.PROCESS_SPEC happy_var_1 happy_var_3 happy_var_5 happy_var_7 happy_var_10
	) `HappyStk` happyRest

happyReduce_85 = happySpecReduce_0  73 happyReduction_85
happyReduction_85  =  HappyAbsSyn73
		 ([]
	)

happyReduce_86 = happySpecReduce_1  73 happyReduction_86
happyReduction_86 (HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn73
		 ((:[]) happy_var_1
	)
happyReduction_86 _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_3  73 happyReduction_87
happyReduction_87 (HappyAbsSyn73  happy_var_3)
	_
	(HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn73
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_87 _ _ _  = notHappyAtAll 

happyReduce_88 = happyReduce 5 74 happyReduction_88
happyReduction_88 (_ `HappyStk`
	(HappyAbsSyn75  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn74
		 (MplAsmLanguage.AbsMPLASM.FUNCTIONS happy_var_4
	) `HappyStk` happyRest

happyReduce_89 = happySpecReduce_0  75 happyReduction_89
happyReduction_89  =  HappyAbsSyn75
		 ([]
	)

happyReduce_90 = happySpecReduce_1  75 happyReduction_90
happyReduction_90 (HappyAbsSyn76  happy_var_1)
	 =  HappyAbsSyn75
		 ((:[]) happy_var_1
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_3  75 happyReduction_91
happyReduction_91 (HappyAbsSyn75  happy_var_3)
	_
	(HappyAbsSyn76  happy_var_1)
	 =  HappyAbsSyn75
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_91 _ _ _  = notHappyAtAll 

happyReduce_92 = happyReduce 6 76 happyReduction_92
happyReduction_92 ((HappyAbsSyn79  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn73  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn76
		 (MplAsmLanguage.AbsMPLASM.FUNCTION_SPEC happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_93 = happyReduce 4 77 happyReduction_93
happyReduction_93 ((HappyAbsSyn79  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn78  happy_var_2) `HappyStk`
	(HappyAbsSyn47  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn77
		 (MplAsmLanguage.AbsMPLASM.MAIN happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_94 = happySpecReduce_0  77 happyReduction_94
happyReduction_94  =  HappyAbsSyn77
		 (MplAsmLanguage.AbsMPLASM.NO_MAIN
	)

happyReduce_95 = happyReduce 6 78 happyReduction_95
happyReduction_95 (_ `HappyStk`
	(HappyAbsSyn73  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn73  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn78
		 (MplAsmLanguage.AbsMPLASM.MAIN_CHANNELS happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_96 = happySpecReduce_3  79 happyReduction_96
happyReduction_96 _
	(HappyAbsSyn80  happy_var_2)
	_
	 =  HappyAbsSyn79
		 (MplAsmLanguage.AbsMPLASM.Prog happy_var_2
	)
happyReduction_96 _ _ _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_0  80 happyReduction_97
happyReduction_97  =  HappyAbsSyn80
		 ([]
	)

happyReduce_98 = happySpecReduce_1  80 happyReduction_98
happyReduction_98 (HappyAbsSyn81  happy_var_1)
	 =  HappyAbsSyn80
		 ((:[]) happy_var_1
	)
happyReduction_98 _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_3  80 happyReduction_99
happyReduction_99 (HappyAbsSyn80  happy_var_3)
	_
	(HappyAbsSyn81  happy_var_1)
	 =  HappyAbsSyn80
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_99 _ _ _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_3  81 happyReduction_100
happyReduction_100 (HappyAbsSyn81  happy_var_3)
	_
	(HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_ASSIGN happy_var_1 happy_var_3
	)
happyReduction_100 _ _ _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_2  81 happyReduction_101
happyReduction_101 (HappyAbsSyn51  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_LOAD happy_var_1 happy_var_2
	)
happyReduction_101 _ _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_1  81 happyReduction_102
happyReduction_102 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_RET happy_var_1
	)
happyReduction_102 _  = notHappyAtAll 

happyReduce_103 = happyReduce 5 81 happyReduction_103
happyReduction_103 (_ `HappyStk`
	(HappyAbsSyn73  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_2) `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_CALL_FUN happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_104 = happySpecReduce_2  81 happyReduction_104
happyReduction_104 (HappyAbsSyn52  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_INT happy_var_1 happy_var_2
	)
happyReduction_104 _ _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_2  81 happyReduction_105
happyReduction_105 (HappyAbsSyn49  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_CHAR happy_var_1 happy_var_2
	)
happyReduction_105 _ _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_2  81 happyReduction_106
happyReduction_106 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_STRING happy_var_1 happy_var_2
	)
happyReduction_106 _ _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_1  81 happyReduction_107
happyReduction_107 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_TOSTR happy_var_1
	)
happyReduction_107 _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_1  81 happyReduction_108
happyReduction_108 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_TOINT happy_var_1
	)
happyReduction_108 _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_1  81 happyReduction_109
happyReduction_109 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_AND happy_var_1
	)
happyReduction_109 _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_1  81 happyReduction_110
happyReduction_110 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_OR happy_var_1
	)
happyReduction_110 _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_1  81 happyReduction_111
happyReduction_111 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_APPEND happy_var_1
	)
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_2  81 happyReduction_112
happyReduction_112 (HappyAbsSyn48  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_BOOL happy_var_1 happy_var_2
	)
happyReduction_112 _ _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_1  81 happyReduction_113
happyReduction_113 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_UNSTRING happy_var_1
	)
happyReduction_113 _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_1  81 happyReduction_114
happyReduction_114 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_LEQ happy_var_1
	)
happyReduction_114 _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_1  81 happyReduction_115
happyReduction_115 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_EQI happy_var_1
	)
happyReduction_115 _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_1  81 happyReduction_116
happyReduction_116 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_LEQC happy_var_1
	)
happyReduction_116 _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_1  81 happyReduction_117
happyReduction_117 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_EQC happy_var_1
	)
happyReduction_117 _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_1  81 happyReduction_118
happyReduction_118 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_LEQS happy_var_1
	)
happyReduction_118 _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_1  81 happyReduction_119
happyReduction_119 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_EQS happy_var_1
	)
happyReduction_119 _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_2  81 happyReduction_120
happyReduction_120 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_CONCAT happy_var_1 happy_var_2
	)
happyReduction_120 _ _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_1  81 happyReduction_121
happyReduction_121 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_ADD happy_var_1
	)
happyReduction_121 _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_1  81 happyReduction_122
happyReduction_122 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_SUB happy_var_1
	)
happyReduction_122 _  = notHappyAtAll 

happyReduce_123 = happySpecReduce_1  81 happyReduction_123
happyReduction_123 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_MUL happy_var_1
	)
happyReduction_123 _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_1  81 happyReduction_124
happyReduction_124 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_DIVQ happy_var_1
	)
happyReduction_124 _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_1  81 happyReduction_125
happyReduction_125 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_DIVR happy_var_1
	)
happyReduction_125 _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_3  81 happyReduction_126
happyReduction_126 (HappyAbsSyn50  happy_var_3)
	_
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_CONSTRUCTOR happy_var_1 happy_var_3
	)
happyReduction_126 _ _ _  = notHappyAtAll 

happyReduce_127 = happyReduce 6 81 happyReduction_127
happyReduction_127 (_ `HappyStk`
	(HappyAbsSyn73  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_CONSTRUCTOR_ARGS happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_128 = happyReduce 6 81 happyReduction_128
happyReduction_128 (_ `HappyStk`
	(HappyAbsSyn84  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_2) `HappyStk`
	(HappyAbsSyn32  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_CASE happy_var_1 happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_129 = happyReduce 6 81 happyReduction_129
happyReduction_129 ((HappyAbsSyn79  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn79  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_2) `HappyStk`
	(HappyAbsSyn33  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_IF happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_130 = happyReduce 5 81 happyReduction_130
happyReduction_130 (_ `HappyStk`
	(HappyAbsSyn84  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_RECORD happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_131 = happyReduce 4 81 happyReduction_131
happyReduction_131 ((HappyAbsSyn51  happy_var_4) `HappyStk`
	(HappyAbsSyn50  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_DEST happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_132 = happyReduce 7 81 happyReduction_132
happyReduction_132 ((HappyAbsSyn51  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn73  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_DEST_ARGS happy_var_1 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_133 = happySpecReduce_3  81 happyReduction_133
happyReduction_133 _
	(HappyAbsSyn73  happy_var_2)
	_
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_PROD happy_var_2
	)
happyReduction_133 _ _ _  = notHappyAtAll 

happyReduce_134 = happyReduce 5 81 happyReduction_134
happyReduction_134 (_ `HappyStk`
	(HappyAbsSyn51  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn52  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_PRODELEM happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_135 = happySpecReduce_1  81 happyReduction_135
happyReduction_135 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_EMSG happy_var_1
	)
happyReduction_135 _  = notHappyAtAll 

happyReduce_136 = happyReduce 4 81 happyReduction_136
happyReduction_136 ((HappyAbsSyn51  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_2) `HappyStk`
	(HappyAbsSyn35  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_GET happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_137 = happyReduce 4 81 happyReduction_137
happyReduction_137 ((HappyAbsSyn51  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_2) `HappyStk`
	(HappyAbsSyn36  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_PUT happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_138 = happyReduce 6 81 happyReduction_138
happyReduction_138 ((HappyAbsSyn51  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_2) `HappyStk`
	(HappyAbsSyn37  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_HPUT happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_139 = happyReduce 6 81 happyReduction_139
happyReduction_139 (_ `HappyStk`
	(HappyAbsSyn84  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_2) `HappyStk`
	(HappyAbsSyn38  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_HCASE happy_var_1 happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_140 = happyReduce 5 81 happyReduction_140
happyReduction_140 ((HappyAbsSyn51  happy_var_5) `HappyStk`
	(HappyAbsSyn51  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_2) `HappyStk`
	(HappyAbsSyn39  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_SPLIT happy_var_1 happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_141 = happyReduce 16 81 happyReduction_141
happyReduction_141 (_ `HappyStk`
	(HappyAbsSyn79  happy_var_15) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn73  happy_var_13) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_11) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn79  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn73  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_2) `HappyStk`
	(HappyAbsSyn40  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_FORK happy_var_1 happy_var_2 happy_var_5 happy_var_7 happy_var_9 happy_var_11 happy_var_13 happy_var_15
	) `HappyStk` happyRest

happyReduce_142 = happyReduce 18 81 happyReduction_142
happyReduction_142 (_ `HappyStk`
	(HappyAbsSyn79  happy_var_17) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn73  happy_var_14) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn79  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn73  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn73  happy_var_2) `HappyStk`
	(HappyAbsSyn41  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_PLUG happy_var_1 happy_var_2 happy_var_7 happy_var_10 happy_var_14 happy_var_17
	) `HappyStk` happyRest

happyReduce_143 = happyReduce 9 81 happyReduction_143
happyReduction_143 (_ `HappyStk`
	(HappyAbsSyn73  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn73  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn73  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn51  happy_var_2) `HappyStk`
	(HappyAbsSyn42  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_RUN happy_var_1 happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_144 = happySpecReduce_3  81 happyReduction_144
happyReduction_144 (HappyAbsSyn51  happy_var_3)
	(HappyAbsSyn46  happy_var_2)
	(HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_ID happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_144 _ _ _  = notHappyAtAll 

happyReduce_145 = happyReduce 4 81 happyReduction_145
happyReduction_145 (_ `HappyStk`
	(HappyAbsSyn86  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn43  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_RACE happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_146 = happySpecReduce_2  81 happyReduction_146
happyReduction_146 (HappyAbsSyn51  happy_var_2)
	(HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_CLOSE happy_var_1 happy_var_2
	)
happyReduction_146 _ _  = notHappyAtAll 

happyReduce_147 = happySpecReduce_2  81 happyReduction_147
happyReduction_147 (HappyAbsSyn51  happy_var_2)
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn81
		 (MplAsmLanguage.AbsMPLASM.AC_HALT happy_var_1 happy_var_2
	)
happyReduction_147 _ _  = notHappyAtAll 

happyReduce_148 = happyReduce 5 82 happyReduction_148
happyReduction_148 ((HappyAbsSyn79  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn82
		 (MplAsmLanguage.AbsMPLASM.AC_LABELLED_COMS_NO_ARGS happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_149 = happyReduce 8 82 happyReduction_149
happyReduction_149 ((HappyAbsSyn79  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn73  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn82
		 (MplAsmLanguage.AbsMPLASM.AC_LABELLED_COMS happy_var_1 happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_150 = happySpecReduce_0  83 happyReduction_150
happyReduction_150  =  HappyAbsSyn83
		 ([]
	)

happyReduce_151 = happySpecReduce_1  83 happyReduction_151
happyReduction_151 (HappyAbsSyn79  happy_var_1)
	 =  HappyAbsSyn83
		 ((:[]) happy_var_1
	)
happyReduction_151 _  = notHappyAtAll 

happyReduce_152 = happySpecReduce_3  83 happyReduction_152
happyReduction_152 (HappyAbsSyn83  happy_var_3)
	_
	(HappyAbsSyn79  happy_var_1)
	 =  HappyAbsSyn83
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_152 _ _ _  = notHappyAtAll 

happyReduce_153 = happySpecReduce_0  84 happyReduction_153
happyReduction_153  =  HappyAbsSyn84
		 ([]
	)

happyReduce_154 = happySpecReduce_1  84 happyReduction_154
happyReduction_154 (HappyAbsSyn82  happy_var_1)
	 =  HappyAbsSyn84
		 ((:[]) happy_var_1
	)
happyReduction_154 _  = notHappyAtAll 

happyReduce_155 = happySpecReduce_3  84 happyReduction_155
happyReduction_155 (HappyAbsSyn84  happy_var_3)
	_
	(HappyAbsSyn82  happy_var_1)
	 =  HappyAbsSyn84
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_155 _ _ _  = notHappyAtAll 

happyReduce_156 = happySpecReduce_3  85 happyReduction_156
happyReduction_156 (HappyAbsSyn79  happy_var_3)
	_
	(HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn85
		 (MplAsmLanguage.AbsMPLASM.AC_RACE_PHRASE happy_var_1 happy_var_3
	)
happyReduction_156 _ _ _  = notHappyAtAll 

happyReduce_157 = happySpecReduce_0  86 happyReduction_157
happyReduction_157  =  HappyAbsSyn86
		 ([]
	)

happyReduce_158 = happySpecReduce_1  86 happyReduction_158
happyReduction_158 (HappyAbsSyn85  happy_var_1)
	 =  HappyAbsSyn86
		 ((:[]) happy_var_1
	)
happyReduction_158 _  = notHappyAtAll 

happyReduce_159 = happySpecReduce_3  86 happyReduction_159
happyReduction_159 (HappyAbsSyn86  happy_var_3)
	_
	(HappyAbsSyn85  happy_var_1)
	 =  HappyAbsSyn86
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_159 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 167 167 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 87;
	PT _ (TS _ 2) -> cont 88;
	PT _ (TS _ 3) -> cont 89;
	PT _ (TS _ 4) -> cont 90;
	PT _ (TS _ 5) -> cont 91;
	PT _ (TS _ 6) -> cont 92;
	PT _ (TS _ 7) -> cont 93;
	PT _ (TS _ 8) -> cont 94;
	PT _ (TS _ 9) -> cont 95;
	PT _ (TS _ 10) -> cont 96;
	PT _ (TS _ 11) -> cont 97;
	PT _ (TS _ 12) -> cont 98;
	PT _ (TS _ 13) -> cont 99;
	PT _ (TS _ 14) -> cont 100;
	PT _ (TS _ 15) -> cont 101;
	PT _ (TS _ 16) -> cont 102;
	PT _ (TS _ 17) -> cont 103;
	PT _ (TS _ 18) -> cont 104;
	PT _ (TS _ 19) -> cont 105;
	PT _ (TS _ 20) -> cont 106;
	PT _ (TS _ 21) -> cont 107;
	PT _ (TS _ 22) -> cont 108;
	PT _ (TS _ 23) -> cont 109;
	PT _ (TS _ 24) -> cont 110;
	PT _ (TS _ 25) -> cont 111;
	PT _ (TS _ 26) -> cont 112;
	PT _ (TS _ 27) -> cont 113;
	PT _ (TS _ 28) -> cont 114;
	PT _ (TS _ 29) -> cont 115;
	PT _ (TS _ 30) -> cont 116;
	PT _ (TI happy_dollar_dollar) -> cont 117;
	PT _ (TL happy_dollar_dollar) -> cont 118;
	PT _ (T_Store _) -> cont 119;
	PT _ (T_Load _) -> cont 120;
	PT _ (T_Ret _) -> cont 121;
	PT _ (T_Call _) -> cont 122;
	PT _ (T_CInt _) -> cont 123;
	PT _ (T_CChar _) -> cont 124;
	PT _ (T_CBool _) -> cont 125;
	PT _ (T_CString _) -> cont 126;
	PT _ (T_ToStr _) -> cont 127;
	PT _ (T_ToInt _) -> cont 128;
	PT _ (T_And _) -> cont 129;
	PT _ (T_Or _) -> cont 130;
	PT _ (T_Append _) -> cont 131;
	PT _ (T_Unstring _) -> cont 132;
	PT _ (T_LeqI _) -> cont 133;
	PT _ (T_EqI _) -> cont 134;
	PT _ (T_LeqC _) -> cont 135;
	PT _ (T_EqC _) -> cont 136;
	PT _ (T_Leqs _) -> cont 137;
	PT _ (T_Eqs _) -> cont 138;
	PT _ (T_ConcatS _) -> cont 139;
	PT _ (T_Add _) -> cont 140;
	PT _ (T_Subtract _) -> cont 141;
	PT _ (T_Mul _) -> cont 142;
	PT _ (T_Quot _) -> cont 143;
	PT _ (T_Rem _) -> cont 144;
	PT _ (T_Case _) -> cont 145;
	PT _ (T_If _) -> cont 146;
	PT _ (T_Rec _) -> cont 147;
	PT _ (T_Get _) -> cont 148;
	PT _ (T_Put _) -> cont 149;
	PT _ (T_Hput _) -> cont 150;
	PT _ (T_Hcase _) -> cont 151;
	PT _ (T_Split _) -> cont 152;
	PT _ (T_Fork _) -> cont 153;
	PT _ (T_Plug _) -> cont 154;
	PT _ (T_Run _) -> cont 155;
	PT _ (T_Race _) -> cont 156;
	PT _ (T_Close _) -> cont 157;
	PT _ (T_Halt _) -> cont 158;
	PT _ (T_Ch_Id _) -> cont 159;
	PT _ (T_Main_run _) -> cont 160;
	PT _ (T_BBool _) -> cont 161;
	PT _ (T_Character _) -> cont 162;
	PT _ (T_UIdent _) -> cont 163;
	PT _ (T_PIdent _) -> cont 164;
	PT _ (T_PInteger _) -> cont 165;
	PT _ (T_IIdent _) -> cont 166;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 167 tk tks = happyError' (tks, explist)
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
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn54 z -> happyReturn z; _other -> notHappyAtAll })

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
