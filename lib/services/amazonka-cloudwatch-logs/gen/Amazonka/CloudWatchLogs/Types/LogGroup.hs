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
-- Module      : Amazonka.CloudWatchLogs.Types.LogGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.LogGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a log group.
--
-- /See:/ 'newLogGroup' smart constructor.
data LogGroup = LogGroup'
  { -- | The creation time of the log group, expressed as the number of
    -- milliseconds after Jan 1, 1970 00:00:00 UTC.
    creationTime :: Prelude.Maybe Prelude.Natural,
    -- | The number of metric filters.
    metricFilterCount :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the log group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the log group.
    logGroupName :: Prelude.Maybe Prelude.Text,
    retentionInDays :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log
    -- data.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The number of bytes stored.
    storedBytes :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'logGroup_creationTime' - The creation time of the log group, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- 'metricFilterCount', 'logGroup_metricFilterCount' - The number of metric filters.
--
-- 'arn', 'logGroup_arn' - The Amazon Resource Name (ARN) of the log group.
--
-- 'logGroupName', 'logGroup_logGroupName' - The name of the log group.
--
-- 'retentionInDays', 'logGroup_retentionInDays' - Undocumented member.
--
-- 'kmsKeyId', 'logGroup_kmsKeyId' - The Amazon Resource Name (ARN) of the CMK to use when encrypting log
-- data.
--
-- 'storedBytes', 'logGroup_storedBytes' - The number of bytes stored.
newLogGroup ::
  LogGroup
newLogGroup =
  LogGroup'
    { creationTime = Prelude.Nothing,
      metricFilterCount = Prelude.Nothing,
      arn = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      retentionInDays = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      storedBytes = Prelude.Nothing
    }

-- | The creation time of the log group, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
logGroup_creationTime :: Lens.Lens' LogGroup (Prelude.Maybe Prelude.Natural)
logGroup_creationTime = Lens.lens (\LogGroup' {creationTime} -> creationTime) (\s@LogGroup' {} a -> s {creationTime = a} :: LogGroup)

-- | The number of metric filters.
logGroup_metricFilterCount :: Lens.Lens' LogGroup (Prelude.Maybe Prelude.Int)
logGroup_metricFilterCount = Lens.lens (\LogGroup' {metricFilterCount} -> metricFilterCount) (\s@LogGroup' {} a -> s {metricFilterCount = a} :: LogGroup)

-- | The Amazon Resource Name (ARN) of the log group.
logGroup_arn :: Lens.Lens' LogGroup (Prelude.Maybe Prelude.Text)
logGroup_arn = Lens.lens (\LogGroup' {arn} -> arn) (\s@LogGroup' {} a -> s {arn = a} :: LogGroup)

-- | The name of the log group.
logGroup_logGroupName :: Lens.Lens' LogGroup (Prelude.Maybe Prelude.Text)
logGroup_logGroupName = Lens.lens (\LogGroup' {logGroupName} -> logGroupName) (\s@LogGroup' {} a -> s {logGroupName = a} :: LogGroup)

-- | Undocumented member.
logGroup_retentionInDays :: Lens.Lens' LogGroup (Prelude.Maybe Prelude.Int)
logGroup_retentionInDays = Lens.lens (\LogGroup' {retentionInDays} -> retentionInDays) (\s@LogGroup' {} a -> s {retentionInDays = a} :: LogGroup)

-- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log
-- data.
logGroup_kmsKeyId :: Lens.Lens' LogGroup (Prelude.Maybe Prelude.Text)
logGroup_kmsKeyId = Lens.lens (\LogGroup' {kmsKeyId} -> kmsKeyId) (\s@LogGroup' {} a -> s {kmsKeyId = a} :: LogGroup)

-- | The number of bytes stored.
logGroup_storedBytes :: Lens.Lens' LogGroup (Prelude.Maybe Prelude.Natural)
logGroup_storedBytes = Lens.lens (\LogGroup' {storedBytes} -> storedBytes) (\s@LogGroup' {} a -> s {storedBytes = a} :: LogGroup)

instance Core.FromJSON LogGroup where
  parseJSON =
    Core.withObject
      "LogGroup"
      ( \x ->
          LogGroup'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "metricFilterCount")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "logGroupName")
            Prelude.<*> (x Core..:? "retentionInDays")
            Prelude.<*> (x Core..:? "kmsKeyId")
            Prelude.<*> (x Core..:? "storedBytes")
      )

instance Prelude.Hashable LogGroup where
  hashWithSalt salt' LogGroup' {..} =
    salt' `Prelude.hashWithSalt` storedBytes
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` retentionInDays
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` metricFilterCount
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData LogGroup where
  rnf LogGroup' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf storedBytes
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf retentionInDays
      `Prelude.seq` Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf metricFilterCount
