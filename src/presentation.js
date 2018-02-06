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
    quartenary: '#CECECE',
  },
  {
    primary: 'Montserrat',
    secondary: 'Helvetica',
  }
);

const cs = {
  inOut: {bg: 'tertiary', h: 'primary', t: 'secondary', p: 'primary'},
  p1: {bg: 'primary', h: 'tertiary', t: 'secondary', p: 'tertiary'},
  p2 : {bg: 'quartenary', h: 'tertiary', t: 'secondary', p: 'tertiary'},
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
            by Pawel Lisewski
          </Text>
        </Slide>

        <Slide bgColor={cs.inOut.bg} textColor={cs.inOut.t}>
          <Image src={require("./assets/img/luke-obiwan-peering-into-lightsaber.jpg")} />
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
            <Quote textColor={cs.p1.h}>The voyage of discovery is not in seeking new landscapes but in having new eyes.</Quote>
            <Cite textColor={cs.p1.t}>Marcel Proust</Cite>
          </BlockQuote>
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
