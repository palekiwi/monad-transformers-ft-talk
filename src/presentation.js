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
  Image,
  Quote,
  Slide,
  Text,
  CodePane,
  S,
  Table,
  TableHeader,
  TableRow,
  TableHeaderItem,
  TableBody,
  TableItem
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
  inOut: {bg: 'primary', h: 'secondary', t: 'tertiary', p: 'quartenary'},
  p1: {bg: 'secondary', h: 'tertiary', t: 'primary', p: 'quartenary'},
  p2: {bg: 'primary', h: 'secondary', t: 'tertiary', p: 'quartenary'},
  p3 : {bg: 'secondary', h: 'tertiary', t: 'primary', p: 'secondary'},
};

const Haskell = (props) =>
  <CodePane
    lang="haskell"
    overflow="overflow"
    margin="10px auto"
    {...props}
    style={{fontSize: '1.4rem'}}
  />

class CodeSlide extends React.Component {
  render() {
    return (
      <Slide bgColor={this.props.colors.bg} progressColor={this.props.colors.t}>
        <Text margin="10px 0 0" textColor={this.props.colors.t} size={1} bold>
          {this.props.title}
        </Text>
        <Haskell source={this.props.source}/>
      </Slide>
    )
  }
}

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
        <Slide bgColor={cs.inOut.bg}>
          <Heading textColor={cs.inOut.h} size={5} bold>
            The Road To
          </Heading>
          <Heading fit size={1} caps lineHeight={1} textColor={cs.inOut.h}>
            Monad Transformers
          </Heading>
          <Text margin="10px 0 0" textColor={cs.inOut.t} size={5} bold>
            by 黎勇禪 Pawel Lisewski
          </Text>
        </Slide>

        <Slide bgColor={cs.inOut.bg} textColor={cs.inOut.t}>
          <Heading fit size={4} caps lineHeight={1} textColor={cs.inOut.h}>
            Why talk about Monad Transformers?
          </Heading>
          <Text margin="10px 0 0" textColor={cs.inOut.t} size={5} bold>
            Is this healthy?
          </Text>
        </Slide>

        <Slide bgColor={cs.inOut.bg} textColor={cs.inOut.t}>
          <Heading margin="10px 0 0" textColor={cs.inOut.t} size={5} bold>
            But why bother?
          </Heading>
          <Image src={require("./assets/img/luke-obiwan-peering-into-lightsaber.jpg")} />
        </Slide>

        <Slide bgColor={cs.inOut.bg} textColor={cs.inOut.t}>
          <Heading size={4} textAlign="left" lineHeight={1} textColor={cs.inOut.t}>
            do
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

        { /* Part I */ }
        <Slide bgColor={cs.p1.bg} textColor={cs.p1.t}>
          <Heading size={3} textColor={cs.p1.t} caps>
            Part I
          </Heading>
          <Heading size={1} textColor={cs.p1.h} caps fit>
            The Base 'Monad'
          </Heading>
        </Slide>

        <Slide bgColor={cs.p1.bg} textColor={cs.p1.t}>
          <BlockQuote>
            <Quote textColor={cs.p1.h}>The true voyage of discovery is not in seeking new landscapes but in having new eyes.</Quote>
            <Cite textColor={cs.p1.t}>Marcel Proust</Cite>
          </BlockQuote>
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
            <ListItem><s strikethrough>makes pure state possible</s></ListItem>
            <ListItem><s strikethrough>makes side effects possible</s></ListItem>
            <ListItem><s strikethrough>makes burritos possible</s></ListItem>
            <ListItem>makes composition possible</ListItem>
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

        { /* Part II */ }
        <Slide bgColor={cs.p2.bg}>
          <Heading size={3} textColor={cs.p2.t} caps>
            Part II
          </Heading>
          <Heading size={1} textColor={cs.p2.h} caps fit>
            The Discovery
          </Heading>
        </Slide>

        <Slide bgColor={cs.p2.bg}>
          <Heading size={3} textColor={cs.p2.t} caps>
            Example: Login shell
          </Heading>
          <Heading size={1} textColor={cs.p2.h} caps fit>
            The Discovery
          </Heading>
        </Slide>

        <Slide bgColor={cs.p2.bg}>
          <Heading size={3} textColor={cs.p2.t} fit caps>
            Example Monad Transformers
          </Heading>

          <Haskell source={require('./assets/MaybeT.hs')} />
          <Haskell source={require('./assets/EitherT.hs')} />
          <Haskell source={require('./assets/WriterT.hs')} />
          <Haskell source={require('./assets/ReaderT.hs')} />
          <Haskell source={require('./assets/StateT.hs')} />
        </Slide>

        { /* Part III */ }
        <Slide bgColor={cs.p3.bg} textColor={cs.p3.t}>
          <Heading size={3} textColor={cs.p3.t} caps>
            Part III
          </Heading>
          <Heading size={1} textColor={cs.p3.h} caps fit>
            The Journey
          </Heading>
        </Slide>

        <Slide bgColor={cs.p3.bg} textColor={cs.p3.t}>
          <BlockQuote>
            <Quote textColor={cs.p3.h}>The familiar becomes new, the new becomes familiar.</Quote>
          </BlockQuote>
        </Slide>

        <CodeSlide title="1st Iteration: Types"
          colors={cs.p3}
          source={require('./assets/server/Main1/types.hs')}/>

        <CodeSlide title="1st Iteration: App State"
          colors={cs.p3}
          source={require('./assets/server/Main1/appstate.hs')}/>

        <CodeSlide title="1st Iteration: Adding Routes"
          colors={cs.p3}
          source={require('./assets/server/Main1/addroute.hs')}/>

        <CodeSlide title="1st Iteration: Folding the Routes"
          colors={cs.p3}
          source={require('./assets/server/Main1/runMyApp.hs')}/>

        { /* Outro */ }
        <Slide bgColor={cs.inOut.bg} textColor={cs.inOut.t}>
          <Heading size={1} textColor={cs.inOut.h} caps fit>
            Outro
          </Heading>
        </Slide>

        <Slide bgColor={cs.inOut.bg} textColor={cs.inOut.t}>
          <BlockQuote>
            <Quote textColor={cs.inOut.h}>Give me six hours to chop a tree and I will spend the first four sharpening the axe.</Quote>
            <Cite textColor={cs.inOut.t}>Abraham Lincoln</Cite>
          </BlockQuote>
        </Slide>

      </Deck>
    );
  }
}
