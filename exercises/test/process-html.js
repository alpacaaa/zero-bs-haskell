
const fs = require('fs')
const glob = require('glob')
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
  content.year = (new Date()).getFullYear()

  return Object.keys(content).reduce((acc, item) => {
    return replaceVar(item, content[item], acc)
  }, tpl)
}


glob('../src/*/', (err, folders) => {
  if (err) throw err

  folders.forEach(exercise => {
    const html = buildHtml(exercise)
    const ex = exercise.split('src/').pop().substr(0, 4)
    const dest = `dist/${ex}`

    fs.mkdirSync(dest)
    fs.writeFileSync(`${dest}/index.html`, html)
  })
})
