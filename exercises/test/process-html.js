
const fs = require('fs')
const YAML = require('yaml')

const tpl = fs.readFileSync('template.html', 'utf-8')

const replaceVar = (k, v, html) => {
  const search = `{{ .${k} }}`
  return html.split(search).join(v)
}

const buildHtml = (exercise) => {
  const data = fs.readFileSync(exercise + 'data.yml', 'utf-8')

  const content = YAML.parse(data)
  content.JS = fs.readFileSync(exercise + 'reference.js')

  return Object.keys(content).reduce((acc, item) => {
    return replaceVar(item, content[item], acc)
  }, tpl)
}


const exercise = '../src/Ex01StaticString/'

console.log(buildHtml(exercise))