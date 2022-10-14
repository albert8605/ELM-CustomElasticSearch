module CustomElasticSearch exposing ( .. )

{-| Easily generate typesafe elasticsearch queries

@docs Query, SearchRequest, searchRequest


# Value types

@docs Value, date, float, int, string


# Comparison operators

These operators are to be used with the range query

@docs gt, gte, gtn, lt, lte, ltn, gtDate, gteDate, ltDate, lteDate


# Queries


## Leaf queries

@docs range, term, type_


## Compound queries

@docs bool, must, mustNot, should, filter


## Common params

@docs boost


# Sort

@docs sortBy, asc, desc


# Encode

@docs encodeQuery, encodeSearchRequest

-}

import Json.Encode as JE
import Time exposing ( Posix )


type alias AttrName =
    String

type alias BoolQuery =
    { must : List Query
    , mustNot : List Query
    , filter : List Query
    , should : List Query
    , boost : Maybe Float
    , minimumShouldMatch : Maybe Int
    , queryName : Maybe String
    }

type alias  RegexpQuery =
    { field : String
    , regexValue : String
    }

type alias SearchRequest =
    { query : Query
    , sort : List Sort
    }


type alias TermQuery =
    { name : AttrName
    , value : Value
    , boost : Maybe Float
    , queryName : Maybe String
    }

type alias TermsQuery =
    { name : String
    , value : JE.Value
    , boost : Maybe Float
    , queryName : Maybe String
    }
    
type alias MatchQuery = 
    { name : AttrName
    , value : Value
    }
    
type alias WildCardQuery = 
    { name : AttrName
    , value : Value
    }

type alias RangeQuery =
    { name : AttrName
    , from : LowerBound
    , to : UpperBound
    , boost : Maybe Float
    , queryName : Maybe String
    , relation : Maybe Relation

    -- , timezone : TimeZone
    }

type TypeQuery
    = TypeQuery String

{-| A query
-}
type Query
    = Term TermQuery
    | Terms TermsQuery
    | Match MatchQuery
    | WildCard WildCardQuery
    | Range RangeQuery
    | Type TypeQuery
    | Bool BoolQuery
    | MatchAll
    | Exists String
    | Regexp String String
    
type BoolClause
    = Must (List Query)
    | Filter (List Query)
    | Should (List Query) (Maybe Int)
    | MustNot (List Query)
    
type SortOrder
    = Asc
    | Desc

type SortMode
    = SortModeMin
    | SortModeMax
    | SortModeSum
    | SortModeAvg
    | SortModeMedian

type Sort
    = SortByField String SortOrder
    | SortByArrayField String SortOrder SortMode
{-| A search request
-}


type Relation
    = Within
    | Contains
    | Intersects
    | Disjoint

type LowerBound
    = NoLowerBound
    | Gte Value
    | Gt Value


type UpperBound
    = NoUpperBound
    | Lte Value
    | Lt Value

type Value
    = IntValue Int
    | StringValue String
    | FloatValue Float
    | DateValue Posix
    | BooleanValue Bool

encodeObject : List ( String, Maybe JE.Value ) -> JE.Value
encodeObject list =
    list
        |> List.filterMap
            ( \( name, value ) ->
                case value of
                    Just v ->
                        Just ( name, v )

                    Nothing ->
                        Nothing
            )
        |> JE.object


encodeSubObject : List String -> List ( String, Maybe JE.Value ) -> JE.Value
encodeSubObject path attrs =
    List.foldr (\name obj -> JE.object [ ( name, obj ) ])
        (encodeObject attrs)
        path


{-| An opaque type for wrapping values
-}



{-| Build a integer value
-}
int : Int -> Value
int =
    IntValue


{-| Build a string value
-}
string : String -> Value
string =
    StringValue


{-| Build a float value
-}
float : Float -> Value
float =
    FloatValue


{-| Build a date value
-}
date : Posix -> Value
date =
    DateValue
    
boolean : Bool -> Value
boolean = BooleanValue


encodeValue : Value -> JE.Value
encodeValue value =
    case value of
        IntValue v ->
            JE.int v

        StringValue v ->
            JE.string v

        FloatValue v ->
            JE.float v

        DateValue v ->
            Time.posixToMillis v
                |> JE.int
        
        BooleanValue v ->
            JE.bool v


-- type MatchAll = MatchAll

encodeTermQuery : TermQuery -> JE.Value
encodeTermQuery q =
    JE.object
        [ ( "term"
          , JE.object
                [ ( q.name
                  , case ( q.boost, q.queryName ) of
                        ( Nothing, Nothing ) ->
                            encodeValue q.value

                        ( boost_, qname ) ->
                            encodeObject
                                [ ( "value", Just <| encodeValue q.value )
                                , encodeBoost boost_
                                , encodeQueryName qname
                                ]
                  )
                ]
          )
        ]

encodeTermsQuery : TermsQuery -> JE.Value
encodeTermsQuery q =
    JE.object
        [ ( "terms"
          , JE.object
                [ ( q.name
                  , q.value
                  )
                ]
          )
        ]
        
encodeMatchQuery : MatchQuery -> JE.Value
encodeMatchQuery q =
    JE.object
        [ ( "match"
          , JE.object
                [ ( q.name, encodeValue q.value )
                ]
          )
        ]
        
encodeMatchAll : JE.Value
encodeMatchAll =
    JE.object
        [ ( "match_all", JE.object [] )]
        
encodeWildCardQuery : WildCardQuery -> JE.Value
encodeWildCardQuery q =
    JE.object
        [ ( "wildcard"
          , JE.object
                [ ( q.name, encodeValue q.value )
                ]
          )
        ]
   

relationToString : Relation -> String
relationToString rel =
    case rel of
        Within ->
            "within"

        Contains ->
            "contains"

        Intersects ->
            "intersects"

        Disjoint ->
            "disjoint"


encodeRelation : Maybe Relation -> ( String, Maybe JE.Value )
encodeRelation rel =
    ( "relation"
    , Maybe.map (relationToString >> JE.string) rel
    )

{-| Greater Than Nothing
Use for a range with no lower bound

-}
gtn : LowerBound
gtn =
    NoLowerBound


{-| Greater Than or Equal
-}
gte : Value -> LowerBound
gte =
    Gte


{-| Greater Than
-}
gt : Value -> LowerBound
gt =
    Gt


{-| Lesser Than Nothing

Use for a range with no upper bound

-}
ltn : UpperBound
ltn =
    NoUpperBound


{-| Lesser Than or Equal
-}
lte : Value -> UpperBound
lte =
    Lte


{-| Lesser Than
-}
lt : Value -> UpperBound
lt =
    Lt


{-| Greater Than or Equal to a date
-}
gteDate : Posix -> LowerBound
gteDate =
    date >> gte


{-| Greater Than a date
-}
gtDate : Posix -> LowerBound
gtDate =
    date >> gt


{-| Lesser Than or Equal to a date
-}
lteDate : Posix -> UpperBound
lteDate =
    date >> lte


{-| Lesser Than a date
-}
ltDate : Posix -> UpperBound
ltDate =
    date >> lt


encodeUpperBound : UpperBound -> ( String, Maybe JE.Value )
encodeUpperBound bound =
    case bound of
        NoUpperBound ->
            ( "", Nothing )

        Lte value ->
            ( "lte", Just <| encodeValue value )

        Lt value ->
            ( "lt", Just <| encodeValue value )


encodeLowerBound : LowerBound -> ( String, Maybe JE.Value )
encodeLowerBound bound =
    case bound of
        NoLowerBound ->
            ( "", Nothing )

        Gte value ->
            ( "gte", Just <| encodeValue value )

        Gt value ->
            ( "gt", Just <| encodeValue value )


encodeBoost : Maybe Float -> ( String, Maybe JE.Value )
encodeBoost boost_ =
    ( "boost", Maybe.map JE.float boost_ )


encodeQueryName : Maybe String -> ( String, Maybe JE.Value )
encodeQueryName name =
    ( "_name", Maybe.map JE.string name )


encodeMinimumShoudMatch : Maybe Int -> ( String, Maybe JE.Value )
encodeMinimumShoudMatch value =
    ( "minimum_should_match", Maybe.map JE.int value )


encodeRangeQuery : RangeQuery -> JE.Value
encodeRangeQuery q =
    encodeSubObject [ "range", q.name ]
        [ encodeLowerBound q.from
        , encodeUpperBound q.to
        , encodeBoost q.boost
        , encodeRelation q.relation
        ]

emptyBoolQuery : BoolQuery
emptyBoolQuery =
    { must = []
    , mustNot = []
    , filter = []
    , should = []
    , boost = Nothing
    , minimumShouldMatch = Nothing
    , queryName = Nothing
    }

encodeRegexpQuery : RegexpQuery -> JE.Value
encodeRegexpQuery q =
    JE.object [
        ("regexp", JE.object [ ( q.field , JE.string q.regexValue ) ])]


regexp : String -> String -> Query
regexp =
    Regexp

encodeBoolQuery : BoolQuery -> JE.Value
encodeBoolQuery q =
    encodeSubObject [ "bool" ]
        [ ( "must", encodeQueryList q.must )
        , ( "must_not", encodeQueryList q.mustNot )
        , ( "filter", encodeQueryList q.filter )
        , ( "should", encodeQueryList q.should )
        , encodeBoost q.boost
        , encodeQueryName q.queryName
        ]

encodeTypeQuery : TypeQuery -> JE.Value
encodeTypeQuery q =
    case q of
        TypeQuery tname ->
            encodeSubObject [ "type" ]
                [ ( "value", Just <| JE.string tname ) ]

{-| Encode a query to a Json.Encode.Value
-}
encodeQuery : Query -> JE.Value
encodeQuery q =
    case q of
        Term q1 ->
            encodeTermQuery q1

        Terms q1 ->
            encodeTermsQuery q1
            
        Match q1 ->
            encodeMatchQuery q1
            
        WildCard q1 ->
            encodeWildCardQuery q1

        Range q1 ->
            encodeRangeQuery q1

        Type q1 ->
            encodeTypeQuery q1

        Bool q1 ->
            encodeBoolQuery q1
            
        MatchAll -> encodeMatchAll

        Exists q1 -> encodeExists q1
        
        Regexp field regexExp -> encodeRegexpQuery   { regexValue = regexExp, field = field }

encodeExists : String -> JE.Value
encodeExists name =
    JE.object 
        [ ( "exists", JE.object [ ( "field", JE.string name ) ] ) ]

encodeQueryList : List Query -> Maybe JE.Value
encodeQueryList qlist =
    case qlist of
        [] ->
            Nothing

        [ single ] ->
            Just <| encodeQuery single

        qlist1 ->
            qlist1
                |> JE.list encodeQuery
                |> Just


{-| A `term` query

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-term-query.html>

-}
term : AttrName -> Value -> Query
term name value =
    Term <| TermQuery name value Nothing Nothing


terms : String -> JE.Value -> Query
terms name values =
    Terms <| TermsQuery name values Nothing Nothing
    
match : AttrName -> Value -> Query
match name value =
    Match <| MatchQuery name value
    
matchAll : Query
matchAll = MatchAll

exists : AttrName -> Query
exists name =
    Exists name

wildcard : AttrName -> Value -> Query
wildcard name value =
    WildCard <| WildCardQuery name value


{-| A `range` query

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-range-query.html>

-}
range : AttrName -> LowerBound -> UpperBound -> Query
range name from to =
    Range <| RangeQuery name from to Nothing Nothing Nothing


{-| A `type` query

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-type-query.html>

-}
type_ : String -> Query
type_ typename =
    Type <| TypeQuery typename


maybeCombine : Maybe a -> Maybe a -> Maybe a
maybeCombine a b =
    case a of
        Just a1 ->
            Just a1

        _ ->
            b




{-| A `bool` query
-}
bool : List BoolClause -> Query
bool =
    List.foldl
        (\clause q ->
            case clause of
                Must list ->
                    { q
                        | must = q.must ++ list
                    }

                Filter list ->
                    { q
                        | filter = q.filter ++ list
                    }

                Should list minShouldMatch ->
                    { q
                        | should = q.should ++ list
                        , minimumShouldMatch = minShouldMatch
                    }

                MustNot list ->
                    { q
                        | mustNot = q.mustNot ++ list
                    }
        )
        emptyBoolQuery
        >> Bool


{-| A `must` clause for the `bool` constructor
-}
must : List Query -> BoolClause
must =
    Must


{-| A `mustNot` clause for the `bool` constructor
-}
mustNot : List Query -> BoolClause
mustNot =
    MustNot


{-| A `should` clause for the `bool` constructor
-}
should : Maybe Int -> List Query -> BoolClause
should minShouldMatch list =
    Should list minShouldMatch


{-| A `filter` clause for the `bool` constructor
-}
filter : List Query -> BoolClause
filter =
    Filter


{-| Set the query boost

For some queries, like `term`, it is a no-op

-}
boost : Float -> Query -> Query
boost b q_ =
    case q_ of
        Term q ->
            Term { q | boost = Just b }

        Terms q ->
            Terms { q | boost = Just b }
            
        Match q -> 
            Match q
            
        WildCard q ->
            WildCard q

        Range q ->
            Range { q | boost = Just b }

        Type q ->
            Type q

        Bool q ->
            Bool { q | boost = Just b }
            
        MatchAll -> MatchAll
    
        Exists n -> Exists n

        Regexp field regexExp -> Regexp field regexExp




sortOrderToString : SortOrder -> String
sortOrderToString order =
    case order of
        Asc ->
            "asc"

        Desc ->
            "desc"


encodeSortOrder : SortOrder -> JE.Value
encodeSortOrder =
    sortOrderToString
        >> JE.string


{-| Sort in ascending order
-}
asc : SortOrder
asc =
    Asc


{-| Sort in descending order
-}
desc : SortOrder
desc =
    Desc




sortModeToString : SortMode -> String
sortModeToString mode =
    case mode of
        SortModeMin ->
            "min"

        SortModeMax ->
            "max"

        SortModeSum ->
            "sum"

        SortModeAvg ->
            "avg"

        SortModeMedian ->
            "median"


encodeSortMode : SortMode -> JE.Value
encodeSortMode =
    sortModeToString
        >> JE.string


sortModeMin : SortMode
sortModeMin =
    SortModeMin


sortModeMax : SortMode
sortModeMax =
    SortModeMax


sortModeSum : SortMode
sortModeSum =
    SortModeSum


sortModeAvg : SortMode
sortModeAvg =
    SortModeAvg


sortModeMedian : SortMode
sortModeMedian =
    SortModeMedian




encodeSort : Sort -> JE.Value
encodeSort sort =
    case sort of
        SortByField name order ->
            JE.object
                [ ( name, encodeSortOrder order )
                ]

        SortByArrayField name order mode ->
            JE.object
                [ ( name
                  , JE.object
                        [ ( "order", encodeSortOrder order )
                        , ( "mode", encodeSortMode mode )
                        ]
                  )
                ]


{-| Sort on a field
-}
sortBy : String -> SortOrder -> Sort
sortBy =
    SortByField


sortByArray : String -> SortOrder -> SortMode -> Sort
sortByArray =
    SortByArrayField





{-| search Request
-}
searchRequest : List Sort -> Query -> SearchRequest
searchRequest sort query =
    SearchRequest query sort


{-| encode a SearchRequest
-}
encodeSearchRequest : SearchRequest -> JE.Value
encodeSearchRequest request =
    JE.object <|
        [ ( "query", encodeQuery request.query ) ]
            ++ case request.sort of
                [] ->
                    []

                list ->
                    [ ( "sort"
                      , JE.list encodeSort list
                      )
                    ]
