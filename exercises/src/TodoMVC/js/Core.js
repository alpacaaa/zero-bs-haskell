
module.exports = {
  initialState: {
    todos: {},
    nextIndex: 1
  },

  createTodo: (state, todoTitle, todoOrder) => {
    const index = state.nextIndex
    const tId = "" + index
    const newTodo = {
      title: todoTitle,
      completed: false,
      url: "http://localhost:7879/api/" + tId,
      todoId: tId,
      order: todoOrder
    }
    const newState = {
      todos: { ...state.todos, [tId]: newTodo },
      nextIndex: index + 1
    }
    return [newState, newTodo]
  },

  updateTodo: (state, existing, todoTitle, todoCompleted, todoOrder) => {
    const updated = {
      ...existing,
      title: withDefault(existing.title, todoTitle),
      completed: withDefault(existing.completed, todoCompleted),
      order: todoOrder
    }

    const newState = { ...state.todos, [existing.todoId]: updated }

    return [newState, updated]
  },

  deleteTodo: (state, todo) => {
    delete state.todos[todo.todoId]
    return state
  },

  findTodo: (state, tId) => {
    return state.todos[tId]
  },

  stateToList: state => {
    return Object.values(state.todos)
  }
}
