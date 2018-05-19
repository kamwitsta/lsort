{-# OPTIONS_GHC -Wno-tabs #-}

{-
lsort does linguistically aware sorting.
The basic sorting is done using UCA, i.e. noticing diacritics only when there's no other option. Modified letters like ŋ or ʒ are sorted according to their base letter. Cyrillic and Greek are sorted inside Latin. Spaces are ignored. There’s also a set of custom substitutions which can be used i.a. to fix the sorting of characters from the Private Use Area.

TODO
– case folding: do I really want that though?

CHANGELOG
0.1.0	2018.02.05
		first version
-}


import Data.List (sortBy)
-- text 1.2.2.2-1+b1
import Data.Text (pack,replace,Text(),unpack)
-- text-icu 0.7.0.1-6+b2
import Data.Text.ICU (collate,NormalizationMode(NFC,NFKD),normalize,uca)
-- text-icu-translit 0.1.07
import Data.Text.ICU.Translit (trans,transliterate)


-- - subs -------------------------------------------------------------------------------------- <<< -

-- | Extensions to the NFKD normalization.
-- Remember that these are applied *after* the standard normalization, so the substitution must be decomposed as much as possible, as in ë́ → e\776\769 rather than ë\769. They are also applied *before* case folding and transliteration, so case and Cyrillic and Greek can be preserved in the substitution.
--	The reasons for this ordering are as follows. Custom substitutions need to be made after the decomposition so that any modifications made to the transliteration don't require dealing with all the accented variants. They also _can_ be made after the decomposition because it does not destroy any data ("a\769" can be found as easily as "á"). At the same time, substitutions must be made before the transliteration because once a "б" is replaced with "b" it can no longer be safely replaced with anything else.
-- The order of diacritics is outward from the letter.
--	751	down arrowhead below
--	752	up arrowhead below
--	768	grave
--	769	acute
--	770	circumflex
--	771	tilde
--	772	macron
--	774	breve (ă)
--	775	dot
--	776	diaeresis
--	778	ring
--	779	doube acute
--	780	caron
--	781	vertical line
--	783	double grave
--	785	inverted breve (ȃ)
--	787 psili (ἀ)
--	796	half ring below
--	797	up tack below
--	799	plus sign below
--	803	dot below
--	805	ring below
--	808	ogonek
--	809	vertical line below
--	814	breve below (ḫ)
--	815	inverted breve below (̯ )
--	817	line below
--	852	left arrowhead below
--	853	right arrowhead below
--	860	doube breve below (t͜s)
--	865	double inverted breve (n͡g)
subs :: [(String,String)]
subs = [
	(" ",""),
	-- Cyrillic
	("ҡ","к"),
	("ү","у\769"),
	("ҙ","з\806"),
	-- Minion Pro MOD2
	("\61440","e\768\769"),
	("\61441","V\769"),
	("\61442","V\815"),
	("\61443","i\815"),
	("\61444","u\815"),
	("\61445","V\772"),
	("\61446","x\803"),
	("\61447","я\769"),
	("\61448","ə\815"),
	("\61449","ə\769"),
	("\61450","n\805"),
	("\61451","x\770"),
	("\61452","ȣ\776"),
	("\61453","e\776\769"),
	("\61454","b\769"),
	("\61455","f\769"),
	("\61456","v\769"),
	("\61457","y\783"),
	("\61458","y\779"),
	("\61459","y\774"),
	("\61460","y\780"),
	("\61461","ng\865"),
	("\61462","s\780\781"),
	("\61463","ʒ\769"),
	("\61464","d\817\803"),
	("\61465","t\769"),
	("\61466","d\769"),
	("\61467","h\769"),
	("\61468","u\776\774"),
	("\61469","χ\769"),
	("\61470","γ\769"),
	("\61471","ŋ\799"),
	("\61472","ɡ\799"),
	("\61473","a(\772)"),
	("\61474","a\776\751"),
	("\61475","k\785"),
	("\61476","ъ\769"),
	("\61477","o\808"),		-- well, not really
	("\61478","ł\769"),
	("\61479","g\785"),
	("\61480","m\768"),
	("\61481","a\772\774"),
	("\61482","e\772\774"),
	("\61483","i\772\774"),
	("\61484","o\772\774"),
	("\61485","u\772\774"),
	("\61486","y\772\774"),
	("\61487","a\772\768"),
	("\61488","a\772\769"),
	("\61489","e\772\769"),
	("\61490","i\772\769"),
	("\61491","o\772\769"),
	("\61492","u\772\769"),
	("\61493","y\772\769"),
	("\61494","ɔ\769"),
	("\61495","α\772\787"),
	("\61496","ι\772\787"),
	("\61497","υ\772\787"),
	("\61498","ι\772\769"),
	("\61499","h\805"),
	("\61500","e\780\769"),
	("\61501","l\805"),
	("\61502","i\814"),
	("\61503","u\775"),
	("\61504","i\852"),
	("\61505","ı\853"),
	("\61506","u\853"),
	("\61507","c\780\769"),
	("\61508","s\780\769"),
	("\61509","z\780\769"),
	("\61510","ʒ\780\769"),
	("\61511","B\769"),
	("\61512","T\769"),
	("\61513","l\769"),
	("\61514","m\772"),
	("\61515","ʒ\780\772"),
	("\61516","o\776\774"),
	("\61517","i\776\772"),
	("\61518","j\774"),
	("\61519","i\775\772"),
	("\61520","s\774"),
	("\61521","z\772"),
	("\61522","i\775\774"),
	("\61523","u\775\772"),
	("\61524","æ"),
	("\61525","əe"),
	("\61526","-\779"),
	("\61527","-\769"),
	("\61528","-\768"),
	("\61529","a\779"),
	("\61530","N"),
	("\61531","⩾"),
	("\61532","r\805"),
	("\61533","r\772\771"),
	("\61534","u\772\771"),
	("\61535","e\775\771"),
	("\61536","e\775\769"),
	("\61537","i\775\768"),
	("\61538","у\769"),
	("\61539","i\775\771"),
	("\61540","i\778"),
	("\61541","ts\860"),
	("\61542","г\817"),
	("\61543","r\797"),
	("\61544","u\799"),
	("\61545","ɨ\799"),
	("\61546","ɔ\771"),
	("\61547","o\776\770"),
	("\61548","r\805\797"),
	("\61549","l\809"),
	("\61550","m\809"),
	("\61551","n\809"),
	("\61552","z\772"),
	("\61553","x\772"),
	("\61554","ə\768"),
	("\61555","i\772\768"),
	("\61556","a\803\772"),
	("\61557","e\803\772"),
	("\61558","a\772\778"),
	("\61559","e\814"),
	("\61560","ə\785"),
	("\61561","o\814\772"),
	("\61562","ŋ\769"),
	("\61563","V\768"),
	("\61564","V\779"),
	("\61565","V\779R\860"),
	("\61566","o\808\768"),
	("\61567","e\808\772"),
	("\61568","V\772"),
	("\61569","o\779l\860"),
	("\61570","e\808\779"),
	("\61571","ь\768"),
	("\61572","V\781"),
	("\61573","e\772\780"),
	("\61574","a\781"),
	("\61575","a\775\769"),
	("\61576","o\815"),
	("\61577","ø"),			-- well, not really
	("\61578","e\780\779"),
	("\61579","e\780\785"),
	("\61580","o\785l\860"),
	("\61581","e\808\785"),
	("\61582","t\774"),
	("\61583","ɐ\853"),
	("\61584","и\769"),
	("\61585","R\805"),
	("\61586","r\771"),
	("\61587","e\808\771"),
	("\61588","e\808\769"),
	("\61589","e\772\771"),
	("\61590","V\771"),
	("\61591","a\808\769"),
	("\61592","l\771"),
	("\61593","R\771"),
	("\61594","j\769"),
	("\61595","j\768"),
	("\61596","i\772\771"),
	("\61597","i\751"),
	("\61598","p\751"),
	("\61599","ŋ\805"),
	("\61600","y\776\772"),
	("\61601","н\803"),
	("\61602","u\776\803"),
	("\61603","c\780\803"),
	("\61604","ə\768"),
	("\61605","k\852"),
	("\61606","i\775\769"),
	("\61607","j\771"),
	("\61608","ɔ\776"),
	("\61609","ɔ\772"),
	("\61610","k\751"),
	("\61611","a\817"),
	("\61612","i\817"),
	("\61613","л\752"),
	("\61614","a\778\772"),
	("\61615","γ\852"),
	("\61616","t\853"),
	("\61617","o\853\775\774"),
	("\61618","a\853\774"),
	("\61619","r\768\796"),
	("\61620","v\768"),
	("\61621","a\775\853"),
	("\61622","r\796\772"),
	("\61623","ᴅ\796\772"),
	("\61624","u\752\772")
	]

-- --------------------------------------------------------------------------------------------- >>> -


-- = main ====================================================================================== <<< =

main :: IO [()]
main = do
	-- read the data from stdin
	datums <- getContents
	let
		-- split the data into lines, remove duplicates, and pack them into Text
		datPckd = map pack $ lines datums
		-- pack the substitutions into Text
		subPckd = map pack2 subs
		-- make a copy of the data for sorting and normalize it
		tmpNrmd = map (normalize NFKD) datPckd
		-- apply the substitutions to the copy
		tmpSubd = map (doSubs subPckd) tmpNrmd
		-- transliterate the copy (see the comment above subs regarding the order)
		tmpTli1 = map (transliterate $ trans (pack "Cyrillic-Latin")) tmpSubd
		tmpTli2 = map (transliterate $ trans (pack "Greek-Latin")) tmpTli1
		-- zip the modified and the originals, and sort by the former
		tmpSrtd = sortBy collateFst $ zip tmpTli2 datPckd
		-- return the now-sorted originals and compose them for good measure
		res = map (unpack . normalize NFC . snd) tmpSrtd
	-- and print the result
	mapM putStrLn res

-- ============================================================================================= >>> =

-- - collateFst -------------------------------------------------------------------------------- <<< -

-- | Collate tuples using UCA.
collateFst :: (Text,Text) -> (Text,Text) -> Ordering
collateFst (s,_) (t,_)	= collate uca s t

-- --------------------------------------------------------------------------------------------- >>> -
-- - doSubs ------------------------------------------------------------------------------------ <<< -

-- | Apply to a Text a whole list of substitutions.
doSubs :: [(Text,Text)] -> Text -> Text
doSubs [] str		= str
doSubs (s:ss) str	= doSubs ss $ replace (fst s) (snd s) str

-- --------------------------------------------------------------------------------------------- >>> -
-- - pack2 ------------------------------------------------------------------------------------- <<< -

-- | Pack String into Text, for tuples.
pack2 :: (String,String) -> (Text,Text)
pack2 (s1,s2)	= (pack s1, pack s2)

-- --------------------------------------------------------------------------------------------- >>> -
