const Core = require("./Core")
const HTTP = require("./HTTP")
const Server = HTTP.Server

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
getAll = (state, _) => [state, Server.jsonResponse(Core.stateToList(state))]

postTodo = (state, req) =>
  decodeInputOrFail(state, req, input => {
    if (input.title === null) {
      return [state, Server.failureResponse("Empty title")]
    } else {
      const [newState, newTodo] = Core.createTodo(
        state,
        input.title,
        input.order
      )
      return [newState, Server.jsonResponse(newTodo)]
    }
  })

deleteAll = (state, _) => [Core.initialState, Server.stringResponse("ok")]

getTodo = (state, req) =>
  findTodoOrFail(state, req, todo => [state, Server.jsonResponse(todo)])

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
      return [newState, Server.jsonResponse(updated)]
    })
  )

deleteTodo = (state, req) =>
  findTodoOrFail(state, req, todo => [
    Core.deleteTodo(state, todo),
    Server.stringResponse("ok")
  ])

findTodoOrFail = (state, req, cb) => {
  let tId = Server.requestParameter(req, "id")

  if (tId === null) {
    return [state, Server.failureResponse("No :id found in URL")]
  } else {
    let todo = Core.findTodo(state, tId)

    if (todo === null) {
      return [state, Server.failureResponse("Todo not found")]
    } else {
      return cb(todo)
    }
  }
}

decodeInputOrFail = (state, req, cb) => {
  const input = {
    title: req.body.title === undefined ? null : req.body.title,
    completed: req.body.completed === undefined ? null : req.body.completed,
    order: req.body.order === undefined ? null : req.body.order
  }

  return cb(input)
}

main()
