{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Naturals where
import Control.Applicative
import Control.Arrow
import Data.Ratio ((%))
import Data.Maybe (fromMaybe)
import Data.Modable
import Data.Normalize
import Language.Haskell.TH hiding (Pred)
import Test.QuickCheck.Arbitrary
import Text.Chunk
import Text.Parse
import Text.ParserCombinators.Parsec (many1, digit)
import Text.Printf

-- | A class for natural numbers.
class Natural n
    where natural :: n -> Integer

-- | The Zero data type
data Zero = Zero
instance Show Zero where show = const "0"
instance Natural Zero where natural = const 0

-- | A type-system natural. Holds an integer which behaves somewhat like
-- | a member of Z mod n (see instances, below.)
newtype Succ a = Z { nat :: Integer } deriving (PrintfArg, Arbitrary)

instance Natural n => Natural (Succ n)
    where natural = const $ 1 + natural (undefined :: n)

instance Natural n => Enum (Succ n) where
    toEnum = Z . fromIntegral
    fromEnum = fromIntegral . nat

instance Natural n => Bounded (Succ n) where
    minBound = Z 0
    maxBound = Z $ natural (undefined :: n)

-- | Puts the integer back into bounds. The integer in a Succ type is
-- | allowed to go out of bounds. The `normalize` function returns
-- | (o, n) where `o` is the number of overflows and `n` is the Succ type
-- | with the integer normalized to be back in bounds [0,N).
-- | The `normalize` function returns just `n`.
instance Natural n => Normalize (Succ n) where
    isNormal z = z >= minBound && z <= maxBound
    normalize z = (o, x) where
        n = natural z
        x = z `mod` Z n
        o = fromIntegral z `div` fromIntegral n

instance Natural n => Displayable (Succ n) where
    name = show . normal
    number = toInteger . normal

instance Eq (Succ n) where
    (Z x) == (Z y) = x == y

instance Ord (Succ n) where
    (Z x) <= (Z y) = x <= y

instance Natural n => Num (Succ n) where
    (Z x) + (Z y) = Z (x + y)
    (Z x) * (Z y) = Z (x * y)
    (Z x) - (Z y) = Z (x - y)
    fromInteger = Z
    signum = Z . signum . nat
    abs = Z . abs . nat

instance Natural n => Real (Succ n) where
    toRational (Z x) = x % 1

instance Natural n => Integral (Succ n) where
    toInteger (Z x) = x
    quotRem (Z x) (Z y) = (fromInteger *** fromInteger) (quotRem x y)

instance Natural n => Relable (Succ n) where
    type Relative (Succ n) = Maybe (Succ n)
instance Natural n => Modable (Succ n) where
    plus a = maybe a (a+)
    minus a = maybe a (a-)
    clobber = fromMaybe
    like a = maybe True (a ==)
    absify = id
    relify = pure

instance Natural n => Show (Succ n) where
    show z@(Z x) = printf (printf "%%0%dd" $ digits $ normal z) x where
        digits = length . show . natural

instance Natural n => Parse (Succ n) where
    parse = (Z . read) <$> many1 digit

-- The first 256 shortcuts for Succ types.
-- Gross, yes, but until template haskell allows you to assign types to
-- types we need a bunch of type synonyms.
-- For instance, we can't say "type N60 = $(zMod 60)" at the moment,
-- because GHC can't prove that there's not a type loop.
-- Having these synonyms allows us to make times like this:
--
-- type HMS = N24 ::: N60 ::: N60
--
-- instead of needing three newtypes.
type N0  = Zero
type N1  = Succ N0
type N2  = Succ N1
type N3  = Succ N2
type N4  = Succ N3
type N5  = Succ N4
type N6  = Succ N5
type N7  = Succ N6
type N8  = Succ N7
type N9  = Succ N8
type N10 = Succ N9
type N11 = Succ N10
type N12 = Succ N11
type N13 = Succ N12
type N14 = Succ N13
type N15 = Succ N14
type N16 = Succ N15
type N17 = Succ N16
type N18 = Succ N17
type N19 = Succ N18
type N20 = Succ N19
type N21 = Succ N20
type N22 = Succ N21
type N23 = Succ N22
type N24 = Succ N23
type N25 = Succ N24
type N26 = Succ N25
type N27 = Succ N26
type N28 = Succ N27
type N29 = Succ N28
type N30 = Succ N29
type N31 = Succ N30
type N32 = Succ N31
type N33 = Succ N32
type N34 = Succ N33
type N35 = Succ N34
type N36 = Succ N35
type N37 = Succ N36
type N38 = Succ N37
type N39 = Succ N38
type N40 = Succ N39
type N41 = Succ N40
type N42 = Succ N41
type N43 = Succ N42
type N44 = Succ N43
type N45 = Succ N44
type N46 = Succ N45
type N47 = Succ N46
type N48 = Succ N47
type N49 = Succ N48
type N50 = Succ N49
type N51 = Succ N50
type N52 = Succ N51
type N53 = Succ N52
type N54 = Succ N53
type N55 = Succ N54
type N56 = Succ N55
type N57 = Succ N56
type N58 = Succ N57
type N59 = Succ N58
type N60 = Succ N59
type N61 = Succ N60
type N62 = Succ N61
type N63 = Succ N62
type N64 = Succ N63
type N65 = Succ N64
type N66 = Succ N65
type N67 = Succ N66
type N68 = Succ N67
type N69 = Succ N68
type N70 = Succ N69
type N71 = Succ N70
type N72 = Succ N71
type N73 = Succ N72
type N74 = Succ N73
type N75 = Succ N74
type N76 = Succ N75
type N77 = Succ N76
type N78 = Succ N77
type N79 = Succ N78
type N80 = Succ N79
type N81 = Succ N80
type N82 = Succ N81
type N83 = Succ N82
type N84 = Succ N83
type N85 = Succ N84
type N86 = Succ N85
type N87 = Succ N86
type N88 = Succ N87
type N89 = Succ N88
type N90 = Succ N89
type N91 = Succ N90
type N92 = Succ N91
type N93 = Succ N92
type N94 = Succ N93
type N95 = Succ N94
type N96 = Succ N95
type N97 = Succ N96
type N98 = Succ N97
type N99 = Succ N98
type N100 = Succ N99
type N101 = Succ N100
type N102 = Succ N101
type N103 = Succ N102
type N104 = Succ N103
type N105 = Succ N104
type N106 = Succ N105
type N107 = Succ N106
type N108 = Succ N107
type N109 = Succ N108
type N110 = Succ N109
type N111 = Succ N110
type N112 = Succ N111
type N113 = Succ N112
type N114 = Succ N113
type N115 = Succ N114
type N116 = Succ N115
type N117 = Succ N116
type N118 = Succ N117
type N119 = Succ N118
type N120 = Succ N119
type N121 = Succ N120
type N122 = Succ N121
type N123 = Succ N122
type N124 = Succ N123
type N125 = Succ N124
type N126 = Succ N125
type N127 = Succ N126
type N128 = Succ N127
type N129 = Succ N128
type N130 = Succ N129
type N131 = Succ N130
type N132 = Succ N131
type N133 = Succ N132
type N134 = Succ N133
type N135 = Succ N134
type N136 = Succ N135
type N137 = Succ N136
type N138 = Succ N137
type N139 = Succ N138
type N140 = Succ N139
type N141 = Succ N140
type N142 = Succ N141
type N143 = Succ N142
type N144 = Succ N143
type N145 = Succ N144
type N146 = Succ N145
type N147 = Succ N146
type N148 = Succ N147
type N149 = Succ N148
type N150 = Succ N149
type N151 = Succ N150
type N152 = Succ N151
type N153 = Succ N152
type N154 = Succ N153
type N155 = Succ N154
type N156 = Succ N155
type N157 = Succ N156
type N158 = Succ N157
type N159 = Succ N158
type N160 = Succ N159
type N161 = Succ N160
type N162 = Succ N161
type N163 = Succ N162
type N164 = Succ N163
type N165 = Succ N164
type N166 = Succ N165
type N167 = Succ N166
type N168 = Succ N167
type N169 = Succ N168
type N170 = Succ N169
type N171 = Succ N170
type N172 = Succ N171
type N173 = Succ N172
type N174 = Succ N173
type N175 = Succ N174
type N176 = Succ N175
type N177 = Succ N176
type N178 = Succ N177
type N179 = Succ N178
type N180 = Succ N179
type N181 = Succ N180
type N182 = Succ N181
type N183 = Succ N182
type N184 = Succ N183
type N185 = Succ N184
type N186 = Succ N185
type N187 = Succ N186
type N188 = Succ N187
type N189 = Succ N188
type N190 = Succ N189
type N191 = Succ N190
type N192 = Succ N191
type N193 = Succ N192
type N194 = Succ N193
type N195 = Succ N194
type N196 = Succ N195
type N197 = Succ N196
type N198 = Succ N197
type N199 = Succ N198
type N200 = Succ N199

-- | A template haskell macro for generating more Succ types.
-- | Due to TH limitations, you can't say N60 = $(zMod 60),
-- | you have to instead use $(zMod 60) wherever you would have
-- | used N60 in the first place.
zMod :: Int -> TypeQ
zMod 0 = [t|Zero|]
zMod x = [t|Succ $(zMod $ x-1)|]
