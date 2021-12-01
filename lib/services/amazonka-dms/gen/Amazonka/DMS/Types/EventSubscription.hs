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
-- Module      : Amazonka.DMS.Types.EventSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.EventSubscription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an event notification subscription created by the
-- @CreateEventSubscription@ operation.
--
-- /See:/ 'newEventSubscription' smart constructor.
data EventSubscription = EventSubscription'
  { -- | The status of the DMS event notification subscription.
    --
    -- Constraints:
    --
    -- Can be one of the following: creating | modifying | deleting | active |
    -- no-permission | topic-not-exist
    --
    -- The status \"no-permission\" indicates that DMS no longer has permission
    -- to post to the SNS topic. The status \"topic-not-exist\" indicates that
    -- the topic was deleted after the subscription was created.
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services customer account associated with the DMS event
    -- notification subscription.
    customerAwsId :: Prelude.Maybe Prelude.Text,
    -- | The DMS event notification subscription Id.
    custSubscriptionId :: Prelude.Maybe Prelude.Text,
    -- | The topic ARN of the DMS event notification subscription.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | Boolean value that indicates if the event subscription is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The type of DMS resource that generates events.
    --
    -- Valid values: replication-instance | replication-server | security-group
    -- | replication-task
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | The time the DMS event notification subscription was created.
    subscriptionCreationTime :: Prelude.Maybe Prelude.Text,
    -- | A lists of event categories.
    eventCategoriesList :: Prelude.Maybe [Prelude.Text],
    -- | A list of source Ids for the event subscription.
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
-- 'status', 'eventSubscription_status' - The status of the DMS event notification subscription.
--
-- Constraints:
--
-- Can be one of the following: creating | modifying | deleting | active |
-- no-permission | topic-not-exist
--
-- The status \"no-permission\" indicates that DMS no longer has permission
-- to post to the SNS topic. The status \"topic-not-exist\" indicates that
-- the topic was deleted after the subscription was created.
--
-- 'customerAwsId', 'eventSubscription_customerAwsId' - The Amazon Web Services customer account associated with the DMS event
-- notification subscription.
--
-- 'custSubscriptionId', 'eventSubscription_custSubscriptionId' - The DMS event notification subscription Id.
--
-- 'snsTopicArn', 'eventSubscription_snsTopicArn' - The topic ARN of the DMS event notification subscription.
--
-- 'enabled', 'eventSubscription_enabled' - Boolean value that indicates if the event subscription is enabled.
--
-- 'sourceType', 'eventSubscription_sourceType' - The type of DMS resource that generates events.
--
-- Valid values: replication-instance | replication-server | security-group
-- | replication-task
--
-- 'subscriptionCreationTime', 'eventSubscription_subscriptionCreationTime' - The time the DMS event notification subscription was created.
--
-- 'eventCategoriesList', 'eventSubscription_eventCategoriesList' - A lists of event categories.
--
-- 'sourceIdsList', 'eventSubscription_sourceIdsList' - A list of source Ids for the event subscription.
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
      subscriptionCreationTime = Prelude.Nothing,
      eventCategoriesList = Prelude.Nothing,
      sourceIdsList = Prelude.Nothing
    }

-- | The status of the DMS event notification subscription.
--
-- Constraints:
--
-- Can be one of the following: creating | modifying | deleting | active |
-- no-permission | topic-not-exist
--
-- The status \"no-permission\" indicates that DMS no longer has permission
-- to post to the SNS topic. The status \"topic-not-exist\" indicates that
-- the topic was deleted after the subscription was created.
eventSubscription_status :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_status = Lens.lens (\EventSubscription' {status} -> status) (\s@EventSubscription' {} a -> s {status = a} :: EventSubscription)

-- | The Amazon Web Services customer account associated with the DMS event
-- notification subscription.
eventSubscription_customerAwsId :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_customerAwsId = Lens.lens (\EventSubscription' {customerAwsId} -> customerAwsId) (\s@EventSubscription' {} a -> s {customerAwsId = a} :: EventSubscription)

-- | The DMS event notification subscription Id.
eventSubscription_custSubscriptionId :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_custSubscriptionId = Lens.lens (\EventSubscription' {custSubscriptionId} -> custSubscriptionId) (\s@EventSubscription' {} a -> s {custSubscriptionId = a} :: EventSubscription)

-- | The topic ARN of the DMS event notification subscription.
eventSubscription_snsTopicArn :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_snsTopicArn = Lens.lens (\EventSubscription' {snsTopicArn} -> snsTopicArn) (\s@EventSubscription' {} a -> s {snsTopicArn = a} :: EventSubscription)

-- | Boolean value that indicates if the event subscription is enabled.
eventSubscription_enabled :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Bool)
eventSubscription_enabled = Lens.lens (\EventSubscription' {enabled} -> enabled) (\s@EventSubscription' {} a -> s {enabled = a} :: EventSubscription)

-- | The type of DMS resource that generates events.
--
-- Valid values: replication-instance | replication-server | security-group
-- | replication-task
eventSubscription_sourceType :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_sourceType = Lens.lens (\EventSubscription' {sourceType} -> sourceType) (\s@EventSubscription' {} a -> s {sourceType = a} :: EventSubscription)

-- | The time the DMS event notification subscription was created.
eventSubscription_subscriptionCreationTime :: Lens.Lens' EventSubscription (Prelude.Maybe Prelude.Text)
eventSubscription_subscriptionCreationTime = Lens.lens (\EventSubscription' {subscriptionCreationTime} -> subscriptionCreationTime) (\s@EventSubscription' {} a -> s {subscriptionCreationTime = a} :: EventSubscription)

-- | A lists of event categories.
eventSubscription_eventCategoriesList :: Lens.Lens' EventSubscription (Prelude.Maybe [Prelude.Text])
eventSubscription_eventCategoriesList = Lens.lens (\EventSubscription' {eventCategoriesList} -> eventCategoriesList) (\s@EventSubscription' {} a -> s {eventCategoriesList = a} :: EventSubscription) Prelude.. Lens.mapping Lens.coerced

-- | A list of source Ids for the event subscription.
eventSubscription_sourceIdsList :: Lens.Lens' EventSubscription (Prelude.Maybe [Prelude.Text])
eventSubscription_sourceIdsList = Lens.lens (\EventSubscription' {sourceIdsList} -> sourceIdsList) (\s@EventSubscription' {} a -> s {sourceIdsList = a} :: EventSubscription) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON EventSubscription where
  parseJSON =
    Core.withObject
      "EventSubscription"
      ( \x ->
          EventSubscription'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "CustomerAwsId")
            Prelude.<*> (x Core..:? "CustSubscriptionId")
            Prelude.<*> (x Core..:? "SnsTopicArn")
            Prelude.<*> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "SourceType")
            Prelude.<*> (x Core..:? "SubscriptionCreationTime")
            Prelude.<*> ( x Core..:? "EventCategoriesList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "SourceIdsList" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable EventSubscription where
  hashWithSalt salt' EventSubscription' {..} =
    salt' `Prelude.hashWithSalt` sourceIdsList
      `Prelude.hashWithSalt` eventCategoriesList
      `Prelude.hashWithSalt` subscriptionCreationTime
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
      `Prelude.seq` Prelude.rnf eventCategoriesList
      `Prelude.seq` Prelude.rnf subscriptionCreationTime
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf snsTopicArn
      `Prelude.seq` Prelude.rnf custSubscriptionId
      `Prelude.seq` Prelude.rnf customerAwsId
