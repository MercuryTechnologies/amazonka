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
-- Module      : Amazonka.Kafka.Types.CloudWatchLogs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.CloudWatchLogs where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newCloudWatchLogs' smart constructor.
data CloudWatchLogs = CloudWatchLogs'
  { logGroup :: Prelude.Maybe Prelude.Text,
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLogs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroup', 'cloudWatchLogs_logGroup' - Undocumented member.
--
-- 'enabled', 'cloudWatchLogs_enabled' - Undocumented member.
newCloudWatchLogs ::
  -- | 'enabled'
  Prelude.Bool ->
  CloudWatchLogs
newCloudWatchLogs pEnabled_ =
  CloudWatchLogs'
    { logGroup = Prelude.Nothing,
      enabled = pEnabled_
    }

-- | Undocumented member.
cloudWatchLogs_logGroup :: Lens.Lens' CloudWatchLogs (Prelude.Maybe Prelude.Text)
cloudWatchLogs_logGroup = Lens.lens (\CloudWatchLogs' {logGroup} -> logGroup) (\s@CloudWatchLogs' {} a -> s {logGroup = a} :: CloudWatchLogs)

-- | Undocumented member.
cloudWatchLogs_enabled :: Lens.Lens' CloudWatchLogs Prelude.Bool
cloudWatchLogs_enabled = Lens.lens (\CloudWatchLogs' {enabled} -> enabled) (\s@CloudWatchLogs' {} a -> s {enabled = a} :: CloudWatchLogs)

instance Core.FromJSON CloudWatchLogs where
  parseJSON =
    Core.withObject
      "CloudWatchLogs"
      ( \x ->
          CloudWatchLogs'
            Prelude.<$> (x Core..:? "logGroup")
            Prelude.<*> (x Core..: "enabled")
      )

instance Prelude.Hashable CloudWatchLogs where
  hashWithSalt salt' CloudWatchLogs' {..} =
    salt' `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` logGroup

instance Prelude.NFData CloudWatchLogs where
  rnf CloudWatchLogs' {..} =
    Prelude.rnf logGroup
      `Prelude.seq` Prelude.rnf enabled

instance Core.ToJSON CloudWatchLogs where
  toJSON CloudWatchLogs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("logGroup" Core..=) Prelude.<$> logGroup,
            Prelude.Just ("enabled" Core..= enabled)
          ]
      )
