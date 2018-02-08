/* eslint-disable import/no-webpack-loader-syntax */
// Import React
import React from 'react';

// Import Spectacle Core tags
import {
  BlockQuote,
  Cite,
  Deck,
  Heading,
  ListItem,
  List,
  Link,
  Image,
  Quote,
  Slide,
  Text,
  CodePane
} from 'spectacle';

// Import theme
import createTheme from 'spectacle/lib/themes/default';

// Require CSS
require('normalize.css');

const theme = createTheme(
  {
    primary: 'white',
    secondary: '#1F2022',
    tertiary: '#03A9FC',
    quartenary: '#999',
  },
  {
    primary: 'Montserrat',
    secondary: 'Helvetica',
  }
);

const cs = {
  inOut: {bg: 'primary', h: 'secondary', t: 'tertiary', p: 'tertiary'},
  p1: {bg: 'secondary', h: 'tertiary', t: 'primary', p: 'quartenary'},
  p2: {bg: 'primary', h: 'secondary', t: 'tertiary', p: 'tertiary'},
  p3 : {bg: 'secondary', h: 'tertiary', t: 'primary', p: 'primary'},
};

const Haskell = (props) =>
  <CodePane
    lang="haskell"
    overflow="overflow"
    margin="10px auto"
    {...props}
    style={{fontSize: '1.4rem'}}
  />

export default class Presentation extends React.Component {
  render() {
    return (
      <Deck
        transition={['fade']}
        transitionDuration={500}
        theme={theme}
        progress="bar"
      >

        { /* Intro */ }
        <Slide bgColor={cs.inOut.bg} progressColor={cs.inOut.p}>
          <Heading textColor={cs.inOut.h} size={5} bold>
            The Road To
          </Heading>
          <Heading fit size={1} caps lineHeight={1} textColor={cs.inOut.h}>
            Monad Transformers
          </Heading>
          <Text margin="10px 0 0" textColor={cs.inOut.t} size={5}>
            by 黎勇禪 Pawel Lisewski
          </Text>
        </Slide>

        <Slide bgColor={cs.inOut.bg} progressColor={cs.inOut.p}>
          <Heading fit size={4} caps lineHeight={1} textColor={cs.inOut.h}>
            Why talk about Monad Transformers?
          </Heading>
        </Slide>

        <Slide bgColor={cs.inOut.bg} progressColor={cs.inOut.p}>
          <Heading margin="10px 0 0" textColor={cs.inOut.t} size={5} bold>
            But why listen?
          </Heading>
          <Image src={require("./assets/img/luke-obiwan-peering-into-lightsaber.jpg")} />
        </Slide>

        <Slide bgColor={cs.inOut.bg} progressColor={cs.inOut.p}>
          <Heading size={4} textAlign="left" lineHeight={1} textColor={cs.inOut.t}>
            Do what?
          </Heading>
          <Heading size={4} textAlign="left" caps lineHeight={1} textColor={cs.inOut.h}>
            Part I: The Base >>=
          </Heading>
          <Heading size={4} textAlign="left" caps lineHeight={1} textColor={cs.inOut.h}>
            Part II: The Discovery >>=
          </Heading>
          <Heading size={4} textAlign="left" caps lineHeight={1} textColor={cs.inOut.h}>
            Part III: The Journey
          </Heading>
        </Slide>

        <Slide bgColor={cs.inOut.bg} progressColor={cs.inOut.p}>
          <BlockQuote>
            <Quote textColor={cs.inOut.h}>The true voyage of discovery is not in seeking new landscapes but in having new eyes.</Quote>
            <Cite textColor={cs.inOut.t}>Marcel Proust</Cite>
          </BlockQuote>
        </Slide>

{ /************************* Part I *********************************/ }
        <Slide bgColor={cs.p1.bg} textColor={cs.p1.t}>
          <Heading size={3} textColor={cs.p1.t} caps>
            Part I
          </Heading>
          <Heading size={1} textColor={cs.p1.h} caps fit>
            The Base 'Monad'
          </Heading>
        </Slide>

        <Slide bgColor={cs.p1.bg} textColor={cs.p1.t}>
          <Heading fit size={3} textColor={cs.p1.t} caps>
            A few thoughts about monads:
          </Heading>
          <List>
            <ListItem>very common in modern FP code</ListItem>
            <ListItem>many implementations, e.g. Maybe, Reader, Writer, STM...</ListItem>
            <ListItem>specialized to solve very specific kinds of problems by providing an environment and services</ListItem>
            <ListItem>...</ListItem>
          </List>
        </Slide>

        <Slide bgColor={cs.p1.bg} textColor={cs.p1.t}>
          <Heading fit size={3} textColor={cs.p1.t} caps>
            State Monad: environment and services
          </Heading>
          <Text fit margin="10px 0 0" textColor={cs.inOut.t} size={5} bold>
            (Store the value for later reference, query the value)
          </Text>
          <Haskell source={require('./assets/StateEnvServices.hs')} />
        </Slide>

        <Slide bgColor={cs.p1.bg} textColor={cs.p1.t}>
          <Heading fit size={3} textColor={cs.p1.t} caps>
            The Power Of Monad:
          </Heading>
          <List>
            <ListItem>is about keeping state pure?</ListItem>
            <ListItem>is about side effects?</ListItem>
            <ListItem>is like a burrito?</ListItem>
            <ListItem>makes composition possible?</ListItem>
          </List>
          <Text margin="10px 0 0" textColor={cs.inOut.t} size={5} bold>
          </Text>
          <Haskell source={require('./assets/MonadComposition.hs')} />
        </Slide>

        <Slide bgColor={cs.p1.bg} textColor={cs.p1.t}>
          <Heading fit size={3} textColor={cs.p1.t} caps>
            The Power Of Monad:
          </Heading>
          <List>
            <ListItem>makes composition possible</ListItem>
          </List>
          <Text fit textColor={cs.inOut.t} size={6} bold>
            Monad takes care of threading the stored value through the whole computation
          </Text>
          <Text margin="10px 0 0" textColor={cs.inOut.t} size={5} bold>
          </Text>
          <Haskell source={require('./assets/MonadComposition.hs')} />
        </Slide>

        <Slide bgColor={cs.p1.bg} textColor={cs.p1.t}>
          <Heading fit size={3} textColor={cs.p1.t} caps>
            In other words, formally:
          </Heading>
          <Text margin="10px 0 0" textColor={cs.inOut.t} size={5} bold>
            Monads promote modularity and code reuse by encapsulating often-used computational strategies into single blocks of code that can be used to construct many different computations.
          </Text>
        </Slide>

        <Slide bgColor={cs.p1.bg} textColor={cs.p1.t}>
          <Heading fit size={3} textColor={cs.p1.t} caps>
            Meanwhile in the real world... it's layers and layers:
          </Heading>
          <List>
            <ListItem>talking to DBs</ListItem>
            <ListItem>using upstream APIs</ListItem>
            <ListItem>own business logic with configs and exceptions</ListItem>
            <ListItem>...</ListItem>
          </List>
        </Slide>

        <Slide bgColor={cs.p1.bg} textColor={cs.p1.t}>
          <Heading fit size={3} textColor={cs.p1.t} caps>
            Question:
          </Heading>
          <Text margin="10px 0 0" textColor={cs.inOut.t} size={5}>
            What happens when we need to use different 'monadic services' at the same time?
          </Text>
        </Slide>

{ /************************ Part II *********************************/ }
        <Slide bgColor={cs.p2.bg} progressColor={cs.p2.p}>
          <Heading size={3} textColor={cs.p2.t} caps>
            Part II
          </Heading>
          <Heading size={1} textColor={cs.p2.h} caps fit>
            The Discovery
          </Heading>
        </Slide>

        <Slide bgColor={cs.p2.bg} progressColor={cs.p2.p}>
          <Text margin="10px 0 0" textColor={cs.p2.t} size={1} bold>
            Example: Login Shell
          </Text>
          <Haskell source={require('./assets/LoginShell_TriangleOfDoom.hs')}/>
        </Slide>

        <Slide bgColor={cs.p2.bg} progressColor={cs.p2.p}>
          <Text margin="10px 0 0" textColor={cs.p2.t} size={1} bold>
            The Impossible Join
          </Text>
          <Haskell source={require('./assets/impossible-join.hs')}/>
        </Slide>

        <Slide bgColor={cs.p2.bg} progressColor={cs.p2.p}>
          <Heading size={3} textColor={cs.p2.t} fit caps>
            Anatomy of A Transformer
          </Heading>
          <Text margin="10px 0 0" textColor={cs.p2.h} size={5}>
            A transformer is a way to add the capability of the PRECURSOR monad to the BASE monad.
          </Text>
          <Haskell source={require('./assets/MaybeT_MonadInstance.hs')} />
        </Slide>

        <Slide bgColor={cs.p2.bg} progressColor={cs.p2.p}>
          <Text margin="10px 0 0" textColor={cs.p2.t} size={1} bold>
            Example: Login Shell With A Transformer
          </Text>
          <Haskell source={require('./assets/LoginShell_Transformer.hs')}/>
        </Slide>

        <Slide bgColor={cs.p2.bg} progressColor={cs.p2.p}>
          <Heading size={3} textColor={cs.p2.t} fit caps>
            Example Monad Transformers
          </Heading>

          <Haskell source={require('./assets/MaybeT.hs')} />
          <Haskell source={require('./assets/EitherT.hs')} />
          <Haskell source={require('./assets/WriterT.hs')} />
          <Haskell source={require('./assets/ReaderT.hs')} />
          <Haskell source={require('./assets/StateT.hs')} />
        </Slide>

        <Slide bgColor={cs.p2.bg} progressColor={cs.p2.p}>
          <Heading fit size={3} textColor={cs.p2.t} caps>
            Question:
          </Heading>
          <Text margin="10px 0 0" textColor={cs.p2.t} size={5}>
            Does ordering matter? Do transformers commute?
          </Text>
        </Slide>

{ /*********************** Part III *********************************/ }
        <Slide bgColor={cs.p3.bg} textColor={cs.p3.t} progressColor={cs.p3.p}>
          <Heading size={3} textColor={cs.p3.t} caps>
            Part III
          </Heading>
          <Heading size={1} textColor={cs.p3.h} caps fit>
            The Journey
          </Heading>
        </Slide>

        <Slide bgColor={cs.p3.bg} textColor={cs.p3.t} progressColor={cs.p3.p}>
          <BlockQuote>
            <Quote textColor={cs.p3.h}>The familiar becomes new, the new becomes familiar.</Quote>
          </BlockQuote>
        </Slide>

        <Slide bgColor={cs.p3.bg} textColor={cs.p3.t} progressColor={cs.p3.p}>
          <Heading size={3} textColor={cs.p3.h} caps fit>
            Why a server example?
          </Heading>
          <List>
            <ListItem>a familiar context to try something new</ListItem>
            <ListItem>good example of service layers and composition</ListItem>
            <ListItem>great follow-up resources</ListItem>
          </List>
        </Slide>

    { /************ 1st Iteration *****************/ }

        <Slide bgColor={cs.p3.bg} textColor={cs.p3.t} progressColor={cs.p3.p}>
          <Heading size={3} textColor={cs.p3.t} caps>
            First Iteration
          </Heading>
          <Heading size={1} textColor={cs.p3.h} fit>
            A functional server
          </Heading>
        </Slide>

        <Slide bgColor={cs.p3.bg} textColor={cs.p3.t} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            1st Iteration: Types
          </Text>
          <Haskell source={require('./assets/server/Main1/types.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} textColor={cs.p3.t} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            1st Iteration: Route Actions
          </Text>
          <Haskell source={require('./assets/server/Main1/route-handlers.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            1st Iteration: App State
          </Text>
          <Haskell source={require('./assets/server/Main1/appstate.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            1st Iteration: Adding Routes
          </Text>
          <Haskell source={require('./assets/server/Main1/addroute.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            1st Iteration: Running The App
          </Text>
          <Haskell source={require('./assets/server/Main1/runMyApp.hs')}/>
        </Slide>

    { /************ 2nd Iteration *****************/ }

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Heading size={3} textColor={cs.p3.t} caps>
            Second Iteration
          </Heading>
          <Heading size={1} textColor={cs.p3.h} fit>
            有拜有保庇 (If Things Go Wrong)
          </Heading>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            2nd Iteration: Types
          </Text>
          <Haskell source={require('./assets/server/Main2/types.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            2nd Iteration: Route Actions
          </Text>
          <Haskell source={require('./assets/server/Main2/route-handlers.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            Meet MonadError
          </Text>
          <Haskell source={require('./assets/MonadError.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            2nd Iteration: App State
          </Text>
          <Haskell source={require('./assets/server/Main2/appstate.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            2nd Iteration: Adding Routes
          </Text>
          <Haskell source={require('./assets/server/Main2/addroute.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            2nd Iteration: Running The App
          </Text>
          <Haskell source={require('./assets/server/Main2/runMyApp.hs')}/>
        </Slide>

    { /************ 3rd Iteration *****************/ }

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Heading size={3} textColor={cs.p3.t} caps>
            Third Iteration
          </Heading>
          <Heading size={1} textColor={cs.p3.h} fit>
            Reading The Environment
          </Heading>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            3rd Iteration: Types
          </Text>
          <Haskell source={require('./assets/server/Main3/types.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            3rd Iteration: Route Actions
          </Text>
          <Haskell source={require('./assets/server/Main3/route-handlers.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            Meet MonadTrans
          </Text>
          <Haskell source={require('./assets/MonadTrans.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            3rd Iteration: Adding Routes
          </Text>
          <Haskell source={require('./assets/server/Main3/addroute.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            3rd Iteration: Running Actions
          </Text>
          <Haskell source={require('./assets/server/Main3/runAction.hs')}/>
        </Slide>

    { /************ 4th Iteration *****************/ }

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Heading size={3} textColor={cs.p3.t} caps>
            Fourth Iteration
          </Heading>
          <Heading size={1} textColor={cs.p3.h} fit>
            The State of Response
          </Heading>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            4th Iteration: Types
          </Text>
          <Haskell source={require('./assets/server/Main4/types.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            4th Iteration: Route Actions
          </Text>
          <Haskell source={require('./assets/server/Main4/route-handlers.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            4th Iteration: Adding Routes
          </Text>
          <Haskell source={require('./assets/server/Main4/addroute.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            4th Iteration: Running Actions
          </Text>
          <Haskell source={require('./assets/server/Main4/runAction.hs')}/>
        </Slide>

    { /************ 5th Iteration *****************/ }

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Heading size={3} textColor={cs.p3.t} caps>
            Fifth Iteration
          </Heading>
          <Heading size={1} textColor={cs.p3.h} fit>
            GET /launch-the-missiles
          </Heading>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            5th Iteration: Types
          </Text>
          <Haskell source={require('./assets/server/Main5/types.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            Meet MonadIO
          </Text>
          <Haskell source={require('./assets/MonadIO.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            5th Iteration: Route Actions
          </Text>
          <Haskell source={require('./assets/server/Main5/route-handlers.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            5th Iteration: Adding Routes
          </Text>
          <Haskell source={require('./assets/server/Main5/addroute.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            5th Iteration: Running Actions
          </Text>
          <Haskell source={require('./assets/server/Main5/runAction.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            5th Iteration: Running The App
          </Text>
          <Haskell source={require('./assets/server/Main5/runMyApp.hs')}/>
        </Slide>

    { /************ 6th Iteration *****************/ }

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Heading size={3} textColor={cs.p3.t} caps>
            Sixth Iteration
          </Heading>
          <Heading size={1} textColor={cs.p3.h} fit>
            愛拼才會贏? Newtype Deriving
          </Heading>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            6th Iteration: Types
          </Text>
          <Haskell source={require('./assets/server/Main6/types.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            6th Iteration: Route Actions
          </Text>
          <Haskell source={require('./assets/server/Main6/route-handlers.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            6th Iteration: App State
          </Text>
          <Haskell source={require('./assets/server/Main6/appstate.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            6th Iteration: Adding Routes
          </Text>
          <Haskell source={require('./assets/server/Main6/addroute.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            6th Iteration: Running Actions
          </Text>
          <Haskell source={require('./assets/server/Main6/runAction.hs')}/>
        </Slide>

        <Slide bgColor={cs.p3.bg} progressColor={cs.p3.p}>
          <Text margin="10px 0 0" textColor={cs.p3.t} size={1} bold>
            6th Iteration: Running The App
          </Text>
          <Haskell source={require('./assets/server/Main6/runMyApp.hs')}/>
        </Slide>

{ /*********************** Outro *********************************/ }
        <Slide bgColor={cs.inOut.bg} progressColor={cs.inOut.p}>
          <Heading size={1} textColor={cs.inOut.h} caps fit>
            Outro
          </Heading>
        </Slide>

        <Slide bgColor={cs.inOut.bg} progressColor={cs.inOut.p}>
          <BlockQuote>
            <Quote textColor={cs.inOut.h}>Give me six hours to chop a tree and I will spend the first four sharpening the axe.</Quote>
            <Cite textColor={cs.inOut.t}>Abraham Lincoln</Cite>
          </BlockQuote>
        </Slide>

        <Slide bgColor={cs.inOut.bg} progressColor={cs.inOut.p}>
          <Heading size={3} textColor={cs.inOut.h} caps fit>
            What next?
          </Heading>
          <List>
            <ListItem>Write instances of transfomers</ListItem>
            <ListItem>Study Scotty on Hackage</ListItem>
            <ListItem>Check <Link href="devanla.com/read-you-a-scotty.html">Read You A Scotty</Link></ListItem>
            <ListItem>Look into Parsers</ListItem>
            <ListItem>Is there an IO_T transformer?</ListItem>
          </List>
        </Slide>

      </Deck>
    );
  }
}
