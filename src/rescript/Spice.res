type decodeError = {
  path: string,
  message: string,
  value: JSON.t,
}

type result<'a> = result<'a, decodeError>
type decoder<'a> = JSON.t => result<'a>
type encoder<'a> = 'a => JSON.t
type codec<'a> = (encoder<'a>, decoder<'a>)

let error = (~path=?, message, value) => {
  let path = switch path {
  | None => ""
  | Some(s) => s
  }
  Error({path, message, value})
}

let stringToJson = (s): JSON.t => JSON.String(s)
let stringFromJson = j =>
  switch (j: JSON.t) {
  | JSON.String(s) => Ok(s)
  | _ => Error({path: "", message: "Not a string", value: j})
  }

let intToJson = (i:int): JSON.t => JSON.Number(Float.fromInt(i))
let intFromJson = (j:JSON.t):result<int> =>
  switch (j: JSON.t) {
  | JSON.Number(f) =>
    Math.floor(f) == f
      ? Ok(Math.floor(f)->Float.toInt)
      : Error({path: "", message: "Not an integer", value: j})

  | _ => Error({path: "", message: "Not a number", value: j})
  }

let bigintToJson = (i): JSON.t => JSON.Number(BigInt.toFloat(i))

let bigintFromJson = j =>
  switch (j: JSON.t) {
  | JSON.Number(n) => Ok(BigInt.fromFloat(n))
  | _ => error("Not a number", j)
  }

let floatToJson = (v:float): JSON.t => JSON.Number(v)
let floatFromJson = j =>
  switch (j: JSON.t) {
  | JSON.Number(f) => Ok(f)
  | _ => Error({path: "", message: "Not a number", value: j})
  }

let boolToJson = (v): JSON.t =>
  switch v {
  | true => JSON.Boolean(true)
  | false => JSON.Boolean(false)
  }
let boolFromJson = j =>
  switch (j: JSON.t) {
  | JSON.Boolean(true) => Ok(true)
  | JSON.Boolean(false) => Ok(false)
  | _ => Error({path: "", message: "Not a boolean", value: j})
  }

let unitToJson = (): JSON.t => JSON.Number(0.0)
let unitFromJson = _ => Ok()

let arrayToJson = (encoder, arr): JSON.t => JSON.Array(arr->Array.map(encoder))

let arrayFromJson = (decoder, json) =>
  switch (json: JSON.t) {
  | JSON.Array(arr) => arr->Array.reduceWithIndex(Ok([]),(acc,jsonI, i) =>
      switch (acc, decoder(jsonI)) {
      | (Error(_), _) => acc

      | (_, Error({path} as error)) =>
        Error({...error, path: "[" ++ (Int.toString(i) ++ ("]" ++ path))})

      | (Ok(prev), Ok(newVal)) =>
        Ok(Array.concat([newVal], prev))
      }
    )

  | _ => Error({path: "", message: "Not an array", value: json})
  }

let listToJson = (encoder, list): JSON.t => arrayToJson(encoder, Belt.List.toArray(list))

let listFromJson = (decoder, json) =>
  Belt.Result.map(arrayFromJson(decoder, json), Belt.List.fromArray)

let filterOptional = arr =>
  Belt.Array.keepMap(arr, ((k, v)) => switch v {
    | Some(v) => Some(k, v)
    | None => None
  })

let optionToJson = (encoder, opt): option<JSON.t> =>
  switch opt {
  | Some(x) => Some(encoder(x))
  | None => None
  }

let optionFromJson = (decoder, json) =>
  switch (json: JSON.t) {
  | JSON.Null => Ok(None)
  | _ => Belt.Result.map(decoder(json), v => Some(v))
  }

let nullToJson = (encoder, opt): JSON.t =>
  switch opt {
  | Null.Value(x) => encoder(x)
  | Null => JSON.Null
  }

let nullFromJson = (decoder, json) =>
  switch (json: JSON.t) {
  | JSON.Null => Ok(Null.Null)
  | _ => Belt.Result.map(decoder(json), v => Null.Value(v))
  }
Js.null
let resultToJson = (okEncoder, errorEncoder, result): JSON.t => JSON.Array(
  switch result {
  | Ok(v) => [JSON.String("Ok"), okEncoder(v)]
  | Error(e) => [JSON.String("Error"), errorEncoder(e)]
  },
)

let resultFromJson = (okDecoder, errorDecoder, json) =>
  switch (json: JSON.t) {
  | JSON.Array([variantConstructorId, payload]) =>
    switch variantConstructorId {
    | JSON.String("Ok") => okDecoder(payload)->Belt.Result.map(v => Ok(v))

    | JSON.String("Error") =>
      switch errorDecoder(payload) {
      | Ok(v) => Ok(Error(v))
      | Error(e) => Error(e)
      }

    | JSON.String(_) => error("Expected either \"Ok\" or \"Error\"", variantConstructorId)
    | _ => error("Not a string", variantConstructorId)
    }
  | JSON.Array(_) => error("Expected exactly 2 values in array", json)
  | _ => error("Not an array", json)
  }

let dictToJson = (encoder, dict): JSON.t => JSON.Object(dict->Dict.mapValues((a) => encoder(a)))

let dictFromJson = (decoder, json) =>
  switch (json: JSON.t) {
  | JSON.Object(dict) =>
    dict
    ->Dict.toArray
    ->Belt.Array.reduce(Ok(Dict.make()), (acc, (key, value)) =>
      switch (acc, decoder(value)) {
      | (Error(_), _) => acc

      | (_, Error({path} as error)) => Error({...error, path: "." ++ (key ++ path)})

      | (Ok(prev), Ok(newVal)) =>
        let () = prev->Dict.set(key, newVal)
        Ok(prev)
      }
    )
  | _ => Error({path: "", message: "Not a dict", value: json})
  }

module Codecs = {
  include Spice_Codecs
  let string = (stringToJson, stringFromJson)
  let int = (intToJson, intFromJson)
  let bigint = (bigintToJson, bigintFromJson)
  let float = (floatToJson, floatFromJson)
  let bool = (boolToJson, boolFromJson)
  let array = (arrayToJson, arrayFromJson)
  let list = (listToJson, listFromJson)
  let option = (optionToJson, optionFromJson)
  let unit = (unitToJson, unitFromJson)
}
