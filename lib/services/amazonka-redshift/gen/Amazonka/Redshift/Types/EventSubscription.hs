{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Redshift.Types.EventSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.EventSubscription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.Tag

-- | Describes event subscriptions.
--
-- /See:/ 'newEventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { -- | The status of the Amazon Redshift event notification subscription.
    --
    -- Constraints:
    --
    -- -   Can be one of the following: active | no-permission |
    --     topic-not-exist
    --
    -- -   The status \"no-permission\" indicates that Amazon Redshift no
    --     longer has permission to post to the Amazon SNS topic. The status
    --     \"topic-not-exist\" indicates that the topic was deleted after the
    --     subscription was created.
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account associated with the Amazon Redshift
    -- event notification subscription.
    customerAwsId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Redshift event notification subscription.
    custSubscriptionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event
    -- notification subscription.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | A boolean value indicating whether the subscription is enabled; @true@
    -- indicates that the subscription is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The source type of the events returned by the Amazon Redshift event
    -- notification, such as cluster, cluster-snapshot,
    -- cluster-parameter-group, cluster-security-group, or scheduled-action.
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | The event severity specified in the Amazon Redshift event notification
    -- subscription.
    --
    -- Values: ERROR, INFO
    severity :: Prelude.Maybe Prelude.Text,
    -- | The date and time the Amazon Redshift event notification subscription
    -- was created.
    subscriptionCreationTime :: Prelude.Maybe Core.ISO8601,
    -- | The list of Amazon Redshift event categories specified in the event
    -- notification subscription.
    --
    -- Values: Configuration, Management, Monitoring, Security, Pending
    eventCategoriesList :: Prelude.Maybe [Prelude.Text],
    -- | The list of tags for the event subscription.
    tags :: Prelude.Maybe [Tag],
    -- | A list of the sources that publish events to the Amazon Redshift event
    -- notification subscription.
    sourceIdsList :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'eventSubscription_status' - The status of the Amazon Redshift event notification subscription.
--
-- Constraints:
--
-- -   Can be one of the following: active | no-permission |
--     topic-not-exist
--
-- -   The status \"no-permission\" indicates that Amazon Redshift no
--     longer has permission to post to the Amazon SNS topic. The status
--     \"topic-not-exist\" indicates that the topic was deleted after the
--     subscription was created.
--
-- 'customerAwsId', 'eventSubscription_customerAwsId' - The Amazon Web Services account associated with the Amazon Redshift
-- event notification subscription.
--
-- 'custSubscriptionId', 'eventSubscription_custSubscriptionId' - The name of the Amazon Redshift event notification subscription.
--
-- 'snsTopicArn', 'eventSubscription_snsTopicArn' - The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event
-- notification subscription.
--
-- 'enabled', 'eventSubscription_enabled' - A boolean value indicating whether the subscription is enabled; @true@
-- indicates that the subscription is enabled.
--
-- 'sourceType', 'eventSubscription_sourceType' - The source type of the events returned by the Amazon Redshift event
-- notification, such as cluster, cluster-snapshot,
-- cluster-parameter-group, cluster-security-group, or scheduled-action.
--
-- 'severity', 'eventSubscription_severity' - The event severity specified in the Amazon Redshift event notification
-- subscription.
--
-- Values: ERROR, INFO
--
-- 'subscriptionCreationTime', 'eventSubscription_subscriptionCreationTime' - The date and time the Amazon Redshift event notification subscription
-- was created.
--
-- 'eventCategoriesList', 'eventSubscription_eventCategoriesList' - The list of Amazon Redshift event categories specified in the event
-- notification subscription.
--
-- Values: Configuration, Management, Monitoring, Security, Pending
--
-- 'tags', 'eventSubscription_tags' - The list of tags for the event subscription.
--
-- 'sourceIdsList', 'eventSubscription_sourceIdsList' - A list of the sources that publish events to the Amazon Redshift event
-- notification subscription.
newEventSubscription ::
  EventSubscription
newEventSubscription =
  EventSubscription'
    { status = Prelude.Nothing,
      customerAwsId = Prelude.Nothing,
      custSubscriptionId = Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      enabled = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      severity = Prelude.Nothing,
      subscriptionCreationTime = Prelude.Nothing,
      eventCategoriesList = Prelude.Nothing,
      tags = Prelude.Nothing,
      sourceIdsList = Prelude.Nothing
    }

-- | The status of the Amazon Redshift event notification subscription.
--
-- Constraints:
--
-- -   Can be one of the following: active | no-permission |
--     topic-not-exist
--
-- -   The status \"no-permission\" indicates that Amazon Redshift no
--     longer has permission to post to the Amazon SNS topic. The status
--     \"topic-not-exist\" indicates that the topic was deleted after the
--     subscription was created.
eventSubscription_status :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_status = Lens.lens (\EventSubscription' {status} -> status) (\s@EventSubscription' {} a -> s {status = a} :: EventSubscription)

-- | The Amazon Web Services account associated with the Amazon Redshift
-- event notification subscription.
eventSubscription_customerAwsId :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_customerAwsId = Lens.lens (\EventSubscription' {customerAwsId} -> customerAwsId) (\s@EventSubscription' {} a -> s {customerAwsId = a} :: EventSubscription)

-- | The name of the Amazon Redshift event notification subscription.
eventSubscription_custSubscriptionId :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_custSubscriptionId = Lens.lens (\EventSubscription' {custSubscriptionId} -> custSubscriptionId) (\s@EventSubscription' {} a -> s {custSubscriptionId = a} :: EventSubscription)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event
-- notification subscription.
eventSubscription_snsTopicArn :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_snsTopicArn = Lens.lens (\EventSubscription' {snsTopicArn} -> snsTopicArn) (\s@EventSubscription' {} a -> s {snsTopicArn = a} :: EventSubscription)

-- | A boolean value indicating whether the subscription is enabled; @true@
-- indicates that the subscription is enabled.
eventSubscription_enabled :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Bool)
eventSubscription_enabled = Lens.lens (\EventSubscription' {enabled} -> enabled) (\s@EventSubscription' {} a -> s {enabled = a} :: EventSubscription)

-- | The source type of the events returned by the Amazon Redshift event
-- notification, such as cluster, cluster-snapshot,
-- cluster-parameter-group, cluster-security-group, or scheduled-action.
eventSubscription_sourceType :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_sourceType = Lens.lens (\EventSubscription' {sourceType} -> sourceType) (\s@EventSubscription' {} a -> s {sourceType = a} :: EventSubscription)

-- | The event severity specified in the Amazon Redshift event notification
-- subscription.
--
-- Values: ERROR, INFO
eventSubscription_severity :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_severity = Lens.lens (\EventSubscription' {severity} -> severity) (\s@EventSubscription' {} a -> s {severity = a} :: EventSubscription)

-- | The date and time the Amazon Redshift event notification subscription
-- was created.
eventSubscription_subscriptionCreationTime :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.UTCTime)
eventSubscription_subscriptionCreationTime = Lens.lens (\EventSubscription' {subscriptionCreationTime} -> subscriptionCreationTime) (\s@EventSubscription' {} a -> s {subscriptionCreationTime = a} :: EventSubscription) Prelude.. Lens.mapping Core._Time

-- | The list of Amazon Redshift event categories specified in the event
-- notification subscription.
--
-- Values: Configuration, Management, Monitoring, Security, Pending
eventSubscription_eventCategoriesList :: Lens.Lens' EventSubscription (Prelude.Maybe [Prelude.Text])
eventSubscription_eventCategoriesList = Lens.lens (\EventSubscription' {eventCategoriesList} -> eventCategoriesList) (\s@EventSubscription' {} a -> s {eventCategoriesList = a} :: EventSubscription) Prelude.. Lens.mapping Lens.coerced

-- | The list of tags for the event subscription.
eventSubscription_tags :: Lens.Lens' EventSubscription (Prelude.Maybe [Tag])
eventSubscription_tags = Lens.lens (\EventSubscription' {tags} -> tags) (\s@EventSubscription' {} a -> s {tags = a} :: EventSubscription) Prelude.. Lens.mapping Lens.coerced

-- | A list of the sources that publish events to the Amazon Redshift event
-- notification subscription.
eventSubscription_sourceIdsList :: Lens.Lens' EventSubscription (Prelude.Maybe [Prelude.Text])
eventSubscription_sourceIdsList = Lens.lens (\EventSubscription' {sourceIdsList} -> sourceIdsList) (\s@EventSubscription' {} a -> s {sourceIdsList = a} :: EventSubscription) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML EventSubscription where
  parseXML x =
    EventSubscription'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "CustomerAwsId")
      Prelude.<*> (x Core..@? "CustSubscriptionId")
      Prelude.<*> (x Core..@? "SnsTopicArn")
      Prelude.<*> (x Core..@? "Enabled")
      Prelude.<*> (x Core..@? "SourceType")
      Prelude.<*> (x Core..@? "Severity")
      Prelude.<*> (x Core..@? "SubscriptionCreationTime")
      Prelude.<*> ( x Core..@? "EventCategoriesList"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "EventCategory")
                  )
      Prelude.<*> ( x Core..@? "Tags" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Tag")
                  )
      Prelude.<*> ( x Core..@? "SourceIdsList" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "SourceId")
                  )

instance Prelude.Hashable EventSubscription where
  hashWithSalt salt' EventSubscription' {..} =
    salt' `Prelude.hashWithSalt` sourceIdsList
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` eventCategoriesList
      `Prelude.hashWithSalt` subscriptionCreationTime
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` snsTopicArn
      `Prelude.hashWithSalt` custSubscriptionId
      `Prelude.hashWithSalt` customerAwsId
      `Prelude.hashWithSalt` status

instance Prelude.NFData EventSubscription where
  rnf EventSubscription' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf sourceIdsList
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf eventCategoriesList
      `Prelude.seq` Prelude.rnf subscriptionCreationTime
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf snsTopicArn
      `Prelude.seq` Prelude.rnf custSubscriptionId
      `Prelude.seq` Prelude.rnf customerAwsId
