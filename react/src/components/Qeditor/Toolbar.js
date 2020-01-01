import React from 'react';
import { StyleButton } from './FontStyle';
import { QuoteButton } from './Quote';
import { LinkButton } from './Link';
import { ImageButton } from './Image';
import { TwitterButton } from './Twitter';
import { Button } from 'antd';
import { useSlate } from 'slate-react';

const ButtonGroup = Button.Group;
const Toolbar = () => {
  const editor = useSlate();
  return (
    <div>
      <ButtonGroup>
        <StyleButton editor={editor} fontStyle='bold' />
        <StyleButton editor={editor} fontStyle='italic' />
        <StyleButton editor={editor} fontStyle='underline' />
        <StyleButton editor={editor} fontStyle='strikethrough' />
        <QuoteButton editor={editor} />
      </ButtonGroup>
      <ButtonGroup>
        <LinkButton editor={editor} />
        <ImageButton editor={editor} />
        <TwitterButton editor={editor} />
      </ButtonGroup>
    </div>
  );
};

export default Toolbar;
