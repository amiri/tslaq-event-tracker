import React from 'react';
import { useSelected, useFocused } from 'slate-react';
import { css } from 'emotion';
import {
  TwitterTimelineEmbed,
  TwitterTweetEmbed,
  TwitterMomentShare,
} from 'react-twitter-embed';

export const Leaf = props => {
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

export const DefaultElement = props => {
  return <p {...props.attributes}>{props.children}</p>;
};

export const ImageElement = props => {
  const selected = useSelected();
  const focused = useFocused();
  return (
    <div {...props.attributes} contentEditable={false}>
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
      {props.children}
    </div>
  );
};

export const TwitterElement = props => {
  switch (props.element.subtype) {
    case 'status':
      return (
        <>
          <TwitterTweetEmbed tweetId={props.element.id} />
          {props.children}
        </>
      );
    case 'timelines':
      return (
        <div {...props.attributes} contentEditable={false}>
          <TwitterTimelineEmbed
            sourceType='collection'
            screenName={props.element.screenName}
            url={props.element.url}
            id={props.element.id}
          />
          {props.children}
        </div>
      );
    case 'lists':
      return (
        <div {...props.attributes} contentEditable={false}>
          <TwitterTimelineEmbed
            sourceType='list'
            ownerScreenName={props.element.screenName}
            url={props.element.url}
            options={props.element.options}
            slug={props.element.id}
          />
          {props.children}
        </div>
      );
    case 'moments':
      return (
        <div {...props.attributes} contentEditable={false}>
          <TwitterMomentShare momentId={props.element.id} />
          {props.children}
        </div>
      );
    default:
      return <div {...props.attributes} contentEditable={false} />;
  }
};

export const LinkElement = props => {
  return (
    <a {...props.attributes} href={props.element.url}>
      {props.children}
    </a>
  );
};

export const QuoteElement = props => {
  return (
    <blockquote className='blockquote' {...props.attributes}>
      {props.children}
    </blockquote>
  );
};
