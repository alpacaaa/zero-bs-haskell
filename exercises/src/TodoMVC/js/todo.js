const Core = require("./Core")
const HTTP = require("./HTTP")

main = () =>
  HTTP.startServer(Core.initialState, [
    HTTP.get("/api", getAll),
    HTTP.post("/api", postTodo),
    HTTP.delete("/api", deleteAll),
    HTTP.get("/api/:id", getTodo),
    HTTP.patch("/api/:id", patchTodo),
    HTTP.delete("/api/:id", deleteTodo)
  ])

// ------
getAll = (state, _) => [state, HTTP.jsonResponse(Core.stateToList(state))]

postTodo = (state, req) =>
  decodeInputOrFail(state, req, input => {
    if (input.title === null) {
      return [state, HTTP.failureResponse("Empty title")]
    } else {
      const [newState, newTodo] = Core.createTodo(
        state,
        input.title,
        input.order
      )
      return [newState, HTTP.jsonResponse(newTodo)]
    }
  })

deleteAll = (state, _) => [Core.initialState, HTTP.stringResponse("ok")]

getTodo = (state, req) =>
  findTodoOrFail(state, req, todo => [state, HTTP.jsonResponse(todo)])

patchTodo = (state, req) =>
  findTodoOrFail(state, req, existing =>
    decodeInputOrFail(state, req, input => {
      let [newState, updated] = Core.updateTodo(
        state,
        existing,
        input.title,
        input.completed,
        input.order
      )
      return [newState, HTTP.jsonResponse(updated)]
    })
  )

deleteTodo = (state, req) =>
  findTodoOrFail(state, req, todo => [
    Core.deleteTodo(state, todo),
    HTTP.stringResponse("ok")
  ])

findTodoOrFail = (state, req, cb) => {
  let tId = HTTP.requestParameter(req, "id")

  if (tId === null) {
    return [state, HTTP.failureResponse("No :id found in URL")]
  } else {
    let todo = Core.findTodo(state, tId)

    if (todo === null) {
      return [state, HTTP.failureResponse("Todo not found")]
    } else {
      return cb(todo)
    }
  }
}

decodeInputOrFail = (state, req, cb) => {
  const input = req.body
  input.title = input.title === undefined ? null : input.title
  input.completed = input.completed === undefined ? null : input.completed
  input.order = input.order === undefined ? null : input.order

  return cb(input)
}

main()
