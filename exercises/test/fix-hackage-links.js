
const fs = require('fs')

const file = 'exercises/test/dist/docs/Zero-Server.html'
const content = fs.readFileSync(file, 'utf-8')

const hackageLink = 'https://hackage.haskell.org/package/'

const fixHackage = (html) => {
  const search = 'href="../'
  if (html.indexOf(search) == -1) return html

  const [before, after] = split(html, search, 2)
  const rest = split(after, "/", 2)
  const remaining = before + `href="${hackageLink}` + rest[0] + "/docs/" + rest[1]
  return fixHackage(remaining)
}

fs.writeFileSync(file, fixHackage(content))



function split (text, separator, limit) {
  const result = text.split(separator)
  if (result.length > limit) {
    return [result[0], result.slice(1).join(separator)]
  }

  return result
}