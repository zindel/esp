{
module Esp.Lexer where
import Debug.Trace
}

%wrapper "monadUserState"


$whitespace = [\x0009\x000C\x000B\x0020\x00A0\xFEFF]
$line_terminator = [\r\n\x2028\x2029]
$alpha = [a-zA-Z]
$digit = [0-9]
$ident = [A-Za-z_]
$sep = [\;\,\(\)\{\}\<\>]

-- comments
@ml_comment = \/\* ( $line_terminator | . )* \*\/

@op = \=\= | \= | \+ | \- | \* | \. | \/
@string = \" ( [^\"] | \\\" )* \" | ' ( [^'] | \\' )* '

tokens :-
    <0> \/\* {
        \input@(p, _, _, _) len -> startComment p >> begin scComment input len
    }
    <scComment> \*\/ {
        \input len -> do (comment', commentPos') <- endComment
                         alexSetStartCode 0
                         return $ Token commentPos' (TComment comment')
        -- token' TCommentEnd `andBegin` 0
    }
    <scComment> . | $line_terminator {
        \input@(_, _, _, s) len ->
          (appendComment $ take len s) >> skip input len
    }
    <0> $whitespace+ ;
    <0> $line_terminator {
        token' TLineTerm
    }
    <0> @string {
        token' $ TConst Str
    }
    ( \- | \+ )? ($digit+ (\. $digit*)? | \. $digit+) ((e|E) $digit+)? {
        token' $ TConst Num
    }
    <0> $ident ($ident | digit) * {
        token' TIdent
    }
    <0> @op  {
        token' TOp
    }
    <0> $sep  {
        token' TSep
    }
{

data AlexUserState = AlexUserState
                     { comment :: String
                     , commentPos :: AlexPosn
                     }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "" (AlexPn 0 0 0)


data ConstType = Num
               | Bool
               | Str
               | Regexp
               | Null
               | NaN
               | Undefined
               deriving (Eq, Show)

data TokenType = TConst ConstType String
               | TIdent String
               | TOp String
               | TSep String
               | TLineTerm String
               | TComment String
               | TCommentStart String
               | TCommentSymbol String
               | TCommentEnd String
               deriving (Eq, Show)

data Token = Token AlexPosn TokenType
           | TEOF
           deriving (Eq, Show)

alexEOF = return TEOF

token' type' (p, _, _, s) len = return $ Token p $ type' $ take len s

startComment :: AlexPosn -> Alex ()
startComment p = Alex $ \s@AlexState{alex_ust=ust} ->
                          let s' = s {alex_ust = ust {comment = "",
                                                      commentPos = p}}
                              in Right (s', ())

appendComment :: String -> Alex ()
appendComment c = Alex $ \s -> let ust = alex_ust s
                                   comment' = (comment ust) ++ c
                                   s' = s {alex_ust = ust {comment = comment'}}
                               in Right (s', ())

endComment :: Alex (String, AlexPosn)
endComment = Alex $ \s@AlexState{alex_ust=ust} ->
                      let comment' = comment ust
                      in Right (s, ("/*" ++ comment' ++ "*/", commentPos ust))

scanTokens :: String -> Either String [Token]
scanTokens str = runAlex str $ do
  let loop = do tok <- alexMonadScan
                case tok of
                    TEOF -> return [tok]
                    _ -> do toks <- loop
                            return (tok:toks)
  loop

testLexer :: String -> IO()
testLexer input = do
    case scanTokens input of
        Right lst -> mapM_ print lst
        Left err -> print err
}


-- vim: filetype=haskell:
