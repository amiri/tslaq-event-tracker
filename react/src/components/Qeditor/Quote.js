import React from 'react';
import { Editor, Transforms } from 'slate';
import classnames from 'classnames';
import { Button } from 'antd';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faQuoteLeft } from '@fortawesome/free-solid-svg-icons';

export const isQuote = editor => {
  const [match] = Editor.nodes(editor, {
    match: n => n.type === 'quote',
  });
  return !!match;
};

export const toggleQuote = editor => {
  const isSet = isQuote(editor);
  Transforms.setNodes(
    editor,
    { type: isSet ? 'paragraph' : 'quote', children: [] },
    {
      match: n => Editor.isBlock(editor, n),
    },
  );
};

export const QuoteButton = ({ editor }) => {
  return (
    <Button
      size='small'
      onMouseDown={e => {
        e.preventDefault();
        toggleQuote(editor);
      }}
      className={classnames({
        active: isQuote(editor),
      })}
    >
      <FontAwesomeIcon icon={faQuoteLeft} />
    </Button>
  );
};
