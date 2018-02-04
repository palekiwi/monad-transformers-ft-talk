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
    quartenary: '#CECECE',
  },
  {
    primary: 'Montserrat',
    secondary: 'Helvetica',
  }
);

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
      <Slide bgColor="primary">
        <Text margin="10px 0 0" textColor="tertiary" size={1} bold>
          {this.props.title}
        </Text>
        <Haskell source={this.props.src}/>
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

        <Slide bgColor="primary">
          <Heading size={1} fit caps lineHeight={1} textColor="secondary">
            The Road to Monad Transformers
          </Heading>
          <Text margin="10px 0 0" textColor="tertiary" size={1} fit bold>
            open the presentation/index.js file to get started
          </Text>
          <Haskell source={require('./assets/MaybeT.hs')}/>
        </Slide>

        <Slide transition={['fade']} bgColor="tertiary">
          <Heading size={6} textColor="primary" caps>
            Typography
          </Heading>
          <Text size={6} textColor="secondary">
            Standard text
          </Text>
        </Slide>

        <Slide bgColor="primary" textColor="tertiary">
          <Heading size={3} textColor="tertiary" caps>
            Part I
          </Heading>
          <Heading size={1} textColor="secondary" caps fit>
            The Base 'Monad'
          </Heading>
        </Slide>

        <Slide bgColor="secondary" textColor="primary">
          <BlockQuote>
            <Quote>The voyage of discovery is not in seeking new landscapes but in having new eyes.</Quote>
            <Cite>Marcel Proust</Cite>
          </BlockQuote>
        </Slide>

        <Slide bgColor="primary" textColor="tertiary">
          <Heading size={3} textColor="tertiary" caps>
            Part II
          </Heading>
          <Heading size={1} textColor="secondary" caps fit>
            The Discovery
          </Heading>
        </Slide>

        <Slide bgColor="primary" textColor="tertiary">
          <Heading size={3} textColor="tertiary" caps>
            Part III
          </Heading>
          <Heading size={1} textColor="secondary" caps fit>
            The Journey
          </Heading>
        </Slide>

        <Slide bgColor="secondary" progressColor="secondary" textColor="primary">
          <BlockQuote>
            <Quote>The familiar becomes new, the new becomes familiar.</Quote>
            <Cite>Marcel Proust</Cite>
          </BlockQuote>
        </Slide>

        <CodeSlide title="1st Iteration: Types"
          src={require('./assets/server/Main1/types.hs')}/>

        <CodeSlide title="1st Iteration: App State"
          src={require('./assets/server/Main1/appstate.hs')}/>

        <CodeSlide title="1st Iteration: Adding Routes"
          src={require('./assets/server/Main1/addroute.hs')}/>

        <CodeSlide title="1st Iteration: Folding the Routes"
          src={require('./assets/server/Main1/runMyApp.hs')}/>

      </Deck>
    );
  }
}
