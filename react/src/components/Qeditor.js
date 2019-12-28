/* eslint-disable default-case, no-empty */
import React, { useState, useMemo, useCallback } from 'react';
import { createEditor } from 'slate';
import {
  Slate,
  Editable,
  withReact,
  useSelected,
  useFocused,
} from 'slate-react';
import { withHistory } from 'slate-history';
import Toolbar from './Qeditor/Toolbar';
import { withImages } from './Qeditor/Image';
import { css } from 'emotion';

const Leaf = props => {
  return (
    <span
      {...props.attributes}
      style={{
        fontWeight: props.leaf.bold ? 'bold' : 'normal',
        fontStyle: props.leaf.italic ? 'italic' : 'normal',
        textDecorationLine:
          props.leaf.underline && props.leaf.strikethrough
            ? 'underline line-through'
            : props.leaf.underline
            ? 'underline'
            : props.leaf.strikethrough
            ? 'line-through'
            : 'none',
      }}
    >
      {props.children}
    </span>
  );
};

const DefaultElement = props => {
  return <p {...props.attributes}>{props.children}</p>;
};

const ImageElement = props => {
  const selected = useSelected();
  const focused = useFocused();
  return (
    <div {...props.attributes}>
      <div contentEditable={false}>
        <img
          alt=''
          src={props.element.url}
          className={css`
            display: block;
            max-width: 100%;
            max-height: 20em;
            box-shadow: ${selected && focused ? '0 0 0 3px #B4D5FF' : 'none'};
          `}
        />
      </div>
      {props.children}
    </div>
  );
};

const LinkElement = props => {
  return (
    <a {...props.attributes} href={props.element.url}>
      {props.children}
    </a>
  );
};
const QuoteElement = props => {
  return (
    <blockquote className='blockquote' {...props.attributes}>
      {props.children}
    </blockquote>
  );
};

const Qeditor = ({ body, onChange, onBlur }) => {
  const editor = useMemo(
    () => withImages(withHistory(withReact(createEditor()))),
    [],
  );
  const [value, setValue] = useState(body);

  const renderElement = useCallback(props => {
    switch (props.element.type) {
      case 'quote':
        return <QuoteElement {...props} />;
      case 'link':
        return <LinkElement {...props} />;
      case 'image':
        return <ImageElement {...props} />;
      default:
        return <DefaultElement {...props} />;
    }
  }, []);

  const renderLeaf = useCallback(props => {
    return <Leaf {...props} />;
  }, []);

  return (
    <Slate
      editor={editor}
      value={value}
      onChange={change => {
        onChange(change);
        setValue(change);
      }}
      onBlur={onBlur}
    >
      <Toolbar />
      <Editable renderElement={renderElement} renderLeaf={renderLeaf} />
    </Slate>
  );
};

export default Qeditor;
