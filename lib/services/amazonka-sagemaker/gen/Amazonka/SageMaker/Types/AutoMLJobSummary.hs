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
-- Module      : Amazonka.SageMaker.Types.AutoMLJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoMLJobSecondaryStatus
import Amazonka.SageMaker.Types.AutoMLJobStatus
import Amazonka.SageMaker.Types.AutoMLPartialFailureReason

-- | Provides a summary about an AutoML job.
--
-- /See:/ 'newAutoMLJobSummary' smart constructor.
data AutoMLJobSummary = AutoMLJobSummary'
  { -- | The failure reason of an AutoML job.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The list of reasons for partial failures within an AutoML job.
    partialFailureReasons :: Prelude.Maybe (Prelude.NonEmpty AutoMLPartialFailureReason),
    -- | The end time of an AutoML job.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the AutoML job you are requesting.
    autoMLJobName :: Prelude.Text,
    -- | The ARN of the AutoML job.
    autoMLJobArn :: Prelude.Text,
    -- | The status of the AutoML job.
    autoMLJobStatus :: AutoMLJobStatus,
    -- | The secondary status of the AutoML job.
    autoMLJobSecondaryStatus :: AutoMLJobSecondaryStatus,
    -- | When the AutoML job was created.
    creationTime :: Core.POSIX,
    -- | When the AutoML job was last modified.
    lastModifiedTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'autoMLJobSummary_failureReason' - The failure reason of an AutoML job.
--
-- 'partialFailureReasons', 'autoMLJobSummary_partialFailureReasons' - The list of reasons for partial failures within an AutoML job.
--
-- 'endTime', 'autoMLJobSummary_endTime' - The end time of an AutoML job.
--
-- 'autoMLJobName', 'autoMLJobSummary_autoMLJobName' - The name of the AutoML job you are requesting.
--
-- 'autoMLJobArn', 'autoMLJobSummary_autoMLJobArn' - The ARN of the AutoML job.
--
-- 'autoMLJobStatus', 'autoMLJobSummary_autoMLJobStatus' - The status of the AutoML job.
--
-- 'autoMLJobSecondaryStatus', 'autoMLJobSummary_autoMLJobSecondaryStatus' - The secondary status of the AutoML job.
--
-- 'creationTime', 'autoMLJobSummary_creationTime' - When the AutoML job was created.
--
-- 'lastModifiedTime', 'autoMLJobSummary_lastModifiedTime' - When the AutoML job was last modified.
newAutoMLJobSummary ::
  -- | 'autoMLJobName'
  Prelude.Text ->
  -- | 'autoMLJobArn'
  Prelude.Text ->
  -- | 'autoMLJobStatus'
  AutoMLJobStatus ->
  -- | 'autoMLJobSecondaryStatus'
  AutoMLJobSecondaryStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  AutoMLJobSummary
newAutoMLJobSummary
  pAutoMLJobName_
  pAutoMLJobArn_
  pAutoMLJobStatus_
  pAutoMLJobSecondaryStatus_
  pCreationTime_
  pLastModifiedTime_ =
    AutoMLJobSummary'
      { failureReason = Prelude.Nothing,
        partialFailureReasons = Prelude.Nothing,
        endTime = Prelude.Nothing,
        autoMLJobName = pAutoMLJobName_,
        autoMLJobArn = pAutoMLJobArn_,
        autoMLJobStatus = pAutoMLJobStatus_,
        autoMLJobSecondaryStatus =
          pAutoMLJobSecondaryStatus_,
        creationTime = Core._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_
      }

-- | The failure reason of an AutoML job.
autoMLJobSummary_failureReason :: Lens.Lens' AutoMLJobSummary (Prelude.Maybe Prelude.Text)
autoMLJobSummary_failureReason = Lens.lens (\AutoMLJobSummary' {failureReason} -> failureReason) (\s@AutoMLJobSummary' {} a -> s {failureReason = a} :: AutoMLJobSummary)

-- | The list of reasons for partial failures within an AutoML job.
autoMLJobSummary_partialFailureReasons :: Lens.Lens' AutoMLJobSummary (Prelude.Maybe (Prelude.NonEmpty AutoMLPartialFailureReason))
autoMLJobSummary_partialFailureReasons = Lens.lens (\AutoMLJobSummary' {partialFailureReasons} -> partialFailureReasons) (\s@AutoMLJobSummary' {} a -> s {partialFailureReasons = a} :: AutoMLJobSummary) Prelude.. Lens.mapping Lens.coerced

-- | The end time of an AutoML job.
autoMLJobSummary_endTime :: Lens.Lens' AutoMLJobSummary (Prelude.Maybe Prelude.UTCTime)
autoMLJobSummary_endTime = Lens.lens (\AutoMLJobSummary' {endTime} -> endTime) (\s@AutoMLJobSummary' {} a -> s {endTime = a} :: AutoMLJobSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the AutoML job you are requesting.
autoMLJobSummary_autoMLJobName :: Lens.Lens' AutoMLJobSummary Prelude.Text
autoMLJobSummary_autoMLJobName = Lens.lens (\AutoMLJobSummary' {autoMLJobName} -> autoMLJobName) (\s@AutoMLJobSummary' {} a -> s {autoMLJobName = a} :: AutoMLJobSummary)

-- | The ARN of the AutoML job.
autoMLJobSummary_autoMLJobArn :: Lens.Lens' AutoMLJobSummary Prelude.Text
autoMLJobSummary_autoMLJobArn = Lens.lens (\AutoMLJobSummary' {autoMLJobArn} -> autoMLJobArn) (\s@AutoMLJobSummary' {} a -> s {autoMLJobArn = a} :: AutoMLJobSummary)

-- | The status of the AutoML job.
autoMLJobSummary_autoMLJobStatus :: Lens.Lens' AutoMLJobSummary AutoMLJobStatus
autoMLJobSummary_autoMLJobStatus = Lens.lens (\AutoMLJobSummary' {autoMLJobStatus} -> autoMLJobStatus) (\s@AutoMLJobSummary' {} a -> s {autoMLJobStatus = a} :: AutoMLJobSummary)

-- | The secondary status of the AutoML job.
autoMLJobSummary_autoMLJobSecondaryStatus :: Lens.Lens' AutoMLJobSummary AutoMLJobSecondaryStatus
autoMLJobSummary_autoMLJobSecondaryStatus = Lens.lens (\AutoMLJobSummary' {autoMLJobSecondaryStatus} -> autoMLJobSecondaryStatus) (\s@AutoMLJobSummary' {} a -> s {autoMLJobSecondaryStatus = a} :: AutoMLJobSummary)

-- | When the AutoML job was created.
autoMLJobSummary_creationTime :: Lens.Lens' AutoMLJobSummary Prelude.UTCTime
autoMLJobSummary_creationTime = Lens.lens (\AutoMLJobSummary' {creationTime} -> creationTime) (\s@AutoMLJobSummary' {} a -> s {creationTime = a} :: AutoMLJobSummary) Prelude.. Core._Time

-- | When the AutoML job was last modified.
autoMLJobSummary_lastModifiedTime :: Lens.Lens' AutoMLJobSummary Prelude.UTCTime
autoMLJobSummary_lastModifiedTime = Lens.lens (\AutoMLJobSummary' {lastModifiedTime} -> lastModifiedTime) (\s@AutoMLJobSummary' {} a -> s {lastModifiedTime = a} :: AutoMLJobSummary) Prelude.. Core._Time

instance Core.FromJSON AutoMLJobSummary where
  parseJSON =
    Core.withObject
      "AutoMLJobSummary"
      ( \x ->
          AutoMLJobSummary'
            Prelude.<$> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "PartialFailureReasons")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..: "AutoMLJobName")
            Prelude.<*> (x Core..: "AutoMLJobArn")
            Prelude.<*> (x Core..: "AutoMLJobStatus")
            Prelude.<*> (x Core..: "AutoMLJobSecondaryStatus")
            Prelude.<*> (x Core..: "CreationTime")
            Prelude.<*> (x Core..: "LastModifiedTime")
      )

instance Prelude.Hashable AutoMLJobSummary where
  hashWithSalt salt' AutoMLJobSummary' {..} =
    salt' `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` autoMLJobSecondaryStatus
      `Prelude.hashWithSalt` autoMLJobStatus
      `Prelude.hashWithSalt` autoMLJobArn
      `Prelude.hashWithSalt` autoMLJobName
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` partialFailureReasons
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData AutoMLJobSummary where
  rnf AutoMLJobSummary' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf autoMLJobSecondaryStatus
      `Prelude.seq` Prelude.rnf autoMLJobStatus
      `Prelude.seq` Prelude.rnf autoMLJobArn
      `Prelude.seq` Prelude.rnf autoMLJobName
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf partialFailureReasons
