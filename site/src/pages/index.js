import React from "react"
import { Link } from "gatsby"

import Layout from "../components/layout"
import Image from "../components/image"
import SEO from "../components/seo"

const IndexPage = () => (
  <Layout>
    <div class="chapter">
      <SEO title="Home" keywords={[`gatsby`, `application`, `react`]} />

      <header>
        <h1 class="js-title">The prelude sucks ass</h1>
      </header>

      <div class="problem">
        <h2>Problem:</h2>
        <h3>You’ve come up with a great-sounding seed of a musical idea. But now that you have it, you find that you don’t know how to proceed. Maybe it’s a few notes or a few bars, but it’s definitely not long enough to be a finished piece. You keep listening to the promising idea over and over again, unable to see a direction that will get you from here to a song.</h3>
        <p>There are a number of methods for generating many new ideas from the seed of one simple idea. Here is one recipe.</p>
      </div>

      <div class="solution">
        <h2>Solution:</h2>
        <p>Make a single duplicate of the initial idea. Edit this duplicate until you have made only one meaningful change. You’ll ultimately need to use your own taste and intuition to determine what “meaningful” means in the context of this particular idea, but the broad categories to consider are the fundamental parameters of music:</p>
        <ul>
          <li>Sound: changes to the timbre</li>
          <li>Harmony: changes to the chords</li>
          <li>Melody: changes to the foreground line</li>
          <li>Rhythm: changes to the timing of events</li>
          <li>Form: changes to the structure or distribution of smaller-scale components within the idea</li>
        </ul>
        <p>Once you’re satisfied with the change that you’ve made to the duplicated idea, repeat the process. But this time, begin by creating a duplicate of the duplicate (rather than the original). Now make one meaningful change to this third-generation duplicate. Try not to go “backwards”; that is, don’t simply undo the change that you made in the previous generation. Continue this process a number of times (maybe eight or so), each time using the previous variation as the seed for the next one.</p>
        <p>What you have now is a collection of “descendant” ideas, each of which is the direct offspring of the previous idea, but which can all be traced back to the original “ancestor.” Although you’ve made only one meaningful change to each idea, all of them have also inherited the changes from all of the preceding generations and so may become increasingly remote from the original ancestor.</p>
      </div>

      <Link to="/page-2/">Go to page 2</Link>
    </div>
  </Layout>
)

export default IndexPage
