import React from 'react';
import {
  QuoteElement,
  LinkElement,
  ImageElement,
  TwitterElement,
  DefaultElement,
  Leaf,
} from './Elements';

export const renderElement = props => {
  switch (props.element.type) {
    case 'quote':
      return <QuoteElement {...props} />;
    case 'link':
      return <LinkElement {...props} />;
    case 'image':
      return <ImageElement {...props} />;
    case 'twitter':
      return <TwitterElement {...props} />;
    default:
      return <DefaultElement {...props} />;
  }
};

export const renderLeaf = props => {
  return <Leaf {...props} />;
};
