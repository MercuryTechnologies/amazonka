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
-- Module      : Amazonka.SESV2.Types.TopicPreference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.TopicPreference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.SubscriptionStatus

-- | The contact\'s preference for being opted-in to or opted-out of a topic.
--
-- /See:/ 'newTopicPreference' smart constructor.
data TopicPreference = TopicPreference'
  { -- | The name of the topic.
    topicName :: Prelude.Text,
    -- | The contact\'s subscription status to a topic which is either @OPT_IN@
    -- or @OPT_OUT@.
    subscriptionStatus :: SubscriptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicPreference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicName', 'topicPreference_topicName' - The name of the topic.
--
-- 'subscriptionStatus', 'topicPreference_subscriptionStatus' - The contact\'s subscription status to a topic which is either @OPT_IN@
-- or @OPT_OUT@.
newTopicPreference ::
  -- | 'topicName'
  Prelude.Text ->
  -- | 'subscriptionStatus'
  SubscriptionStatus ->
  TopicPreference
newTopicPreference pTopicName_ pSubscriptionStatus_ =
  TopicPreference'
    { topicName = pTopicName_,
      subscriptionStatus = pSubscriptionStatus_
    }

-- | The name of the topic.
topicPreference_topicName :: Lens.Lens' TopicPreference Prelude.Text
topicPreference_topicName = Lens.lens (\TopicPreference' {topicName} -> topicName) (\s@TopicPreference' {} a -> s {topicName = a} :: TopicPreference)

-- | The contact\'s subscription status to a topic which is either @OPT_IN@
-- or @OPT_OUT@.
topicPreference_subscriptionStatus :: Lens.Lens' TopicPreference SubscriptionStatus
topicPreference_subscriptionStatus = Lens.lens (\TopicPreference' {subscriptionStatus} -> subscriptionStatus) (\s@TopicPreference' {} a -> s {subscriptionStatus = a} :: TopicPreference)

instance Core.FromJSON TopicPreference where
  parseJSON =
    Core.withObject
      "TopicPreference"
      ( \x ->
          TopicPreference'
            Prelude.<$> (x Core..: "TopicName")
            Prelude.<*> (x Core..: "SubscriptionStatus")
      )

instance Prelude.Hashable TopicPreference where
  hashWithSalt salt' TopicPreference' {..} =
    salt' `Prelude.hashWithSalt` subscriptionStatus
      `Prelude.hashWithSalt` topicName

instance Prelude.NFData TopicPreference where
  rnf TopicPreference' {..} =
    Prelude.rnf topicName
      `Prelude.seq` Prelude.rnf subscriptionStatus

instance Core.ToJSON TopicPreference where
  toJSON TopicPreference' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TopicName" Core..= topicName),
            Prelude.Just
              ("SubscriptionStatus" Core..= subscriptionStatus)
          ]
      )
