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
-- Module      : Amazonka.DMS.Types.ReplicationTaskIndividualAssessment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.ReplicationTaskIndividualAssessment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information that describes an individual assessment from a
-- premigration assessment run.
--
-- /See:/ 'newReplicationTaskIndividualAssessment' smart constructor.
data ReplicationTaskIndividualAssessment = ReplicationTaskIndividualAssessment'
  { -- | Individual assessment status.
    --
    -- This status can have one of the following values:
    --
    -- -   @\"cancelled\"@
    --
    -- -   @\"error\"@
    --
    -- -   @\"failed\"@
    --
    -- -   @\"passed\"@
    --
    -- -   @\"pending\"@
    --
    -- -   @\"running\"@
    status :: Prelude.Maybe Prelude.Text,
    -- | Date when this individual assessment was started as part of running the
    -- @StartReplicationTaskAssessmentRun@ operation.
    replicationTaskIndividualAssessmentStartDate :: Prelude.Maybe Core.POSIX,
    -- | Name of this individual assessment.
    individualAssessmentName :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of this individual assessment.
    replicationTaskIndividualAssessmentArn :: Prelude.Maybe Prelude.Text,
    -- | ARN of the premigration assessment run that is created to run this
    -- individual assessment.
    replicationTaskAssessmentRunArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationTaskIndividualAssessment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'replicationTaskIndividualAssessment_status' - Individual assessment status.
--
-- This status can have one of the following values:
--
-- -   @\"cancelled\"@
--
-- -   @\"error\"@
--
-- -   @\"failed\"@
--
-- -   @\"passed\"@
--
-- -   @\"pending\"@
--
-- -   @\"running\"@
--
-- 'replicationTaskIndividualAssessmentStartDate', 'replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentStartDate' - Date when this individual assessment was started as part of running the
-- @StartReplicationTaskAssessmentRun@ operation.
--
-- 'individualAssessmentName', 'replicationTaskIndividualAssessment_individualAssessmentName' - Name of this individual assessment.
--
-- 'replicationTaskIndividualAssessmentArn', 'replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentArn' - Amazon Resource Name (ARN) of this individual assessment.
--
-- 'replicationTaskAssessmentRunArn', 'replicationTaskIndividualAssessment_replicationTaskAssessmentRunArn' - ARN of the premigration assessment run that is created to run this
-- individual assessment.
newReplicationTaskIndividualAssessment ::
  ReplicationTaskIndividualAssessment
newReplicationTaskIndividualAssessment =
  ReplicationTaskIndividualAssessment'
    { status =
        Prelude.Nothing,
      replicationTaskIndividualAssessmentStartDate =
        Prelude.Nothing,
      individualAssessmentName =
        Prelude.Nothing,
      replicationTaskIndividualAssessmentArn =
        Prelude.Nothing,
      replicationTaskAssessmentRunArn =
        Prelude.Nothing
    }

-- | Individual assessment status.
--
-- This status can have one of the following values:
--
-- -   @\"cancelled\"@
--
-- -   @\"error\"@
--
-- -   @\"failed\"@
--
-- -   @\"passed\"@
--
-- -   @\"pending\"@
--
-- -   @\"running\"@
replicationTaskIndividualAssessment_status :: Lens.Lens' ReplicationTaskIndividualAssessment (Prelude.Maybe Prelude.Text)
replicationTaskIndividualAssessment_status = Lens.lens (\ReplicationTaskIndividualAssessment' {status} -> status) (\s@ReplicationTaskIndividualAssessment' {} a -> s {status = a} :: ReplicationTaskIndividualAssessment)

-- | Date when this individual assessment was started as part of running the
-- @StartReplicationTaskAssessmentRun@ operation.
replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentStartDate :: Lens.Lens' ReplicationTaskIndividualAssessment (Prelude.Maybe Prelude.UTCTime)
replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentStartDate = Lens.lens (\ReplicationTaskIndividualAssessment' {replicationTaskIndividualAssessmentStartDate} -> replicationTaskIndividualAssessmentStartDate) (\s@ReplicationTaskIndividualAssessment' {} a -> s {replicationTaskIndividualAssessmentStartDate = a} :: ReplicationTaskIndividualAssessment) Prelude.. Lens.mapping Core._Time

-- | Name of this individual assessment.
replicationTaskIndividualAssessment_individualAssessmentName :: Lens.Lens' ReplicationTaskIndividualAssessment (Prelude.Maybe Prelude.Text)
replicationTaskIndividualAssessment_individualAssessmentName = Lens.lens (\ReplicationTaskIndividualAssessment' {individualAssessmentName} -> individualAssessmentName) (\s@ReplicationTaskIndividualAssessment' {} a -> s {individualAssessmentName = a} :: ReplicationTaskIndividualAssessment)

-- | Amazon Resource Name (ARN) of this individual assessment.
replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentArn :: Lens.Lens' ReplicationTaskIndividualAssessment (Prelude.Maybe Prelude.Text)
replicationTaskIndividualAssessment_replicationTaskIndividualAssessmentArn = Lens.lens (\ReplicationTaskIndividualAssessment' {replicationTaskIndividualAssessmentArn} -> replicationTaskIndividualAssessmentArn) (\s@ReplicationTaskIndividualAssessment' {} a -> s {replicationTaskIndividualAssessmentArn = a} :: ReplicationTaskIndividualAssessment)

-- | ARN of the premigration assessment run that is created to run this
-- individual assessment.
replicationTaskIndividualAssessment_replicationTaskAssessmentRunArn :: Lens.Lens' ReplicationTaskIndividualAssessment (Prelude.Maybe Prelude.Text)
replicationTaskIndividualAssessment_replicationTaskAssessmentRunArn = Lens.lens (\ReplicationTaskIndividualAssessment' {replicationTaskAssessmentRunArn} -> replicationTaskAssessmentRunArn) (\s@ReplicationTaskIndividualAssessment' {} a -> s {replicationTaskAssessmentRunArn = a} :: ReplicationTaskIndividualAssessment)

instance
  Core.FromJSON
    ReplicationTaskIndividualAssessment
  where
  parseJSON =
    Core.withObject
      "ReplicationTaskIndividualAssessment"
      ( \x ->
          ReplicationTaskIndividualAssessment'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> ( x
                            Core..:? "ReplicationTaskIndividualAssessmentStartDate"
                        )
            Prelude.<*> (x Core..:? "IndividualAssessmentName")
            Prelude.<*> (x Core..:? "ReplicationTaskIndividualAssessmentArn")
            Prelude.<*> (x Core..:? "ReplicationTaskAssessmentRunArn")
      )

instance
  Prelude.Hashable
    ReplicationTaskIndividualAssessment
  where
  hashWithSalt
    salt'
    ReplicationTaskIndividualAssessment' {..} =
      salt'
        `Prelude.hashWithSalt` replicationTaskAssessmentRunArn
        `Prelude.hashWithSalt` replicationTaskIndividualAssessmentArn
        `Prelude.hashWithSalt` individualAssessmentName
        `Prelude.hashWithSalt` replicationTaskIndividualAssessmentStartDate
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    ReplicationTaskIndividualAssessment
  where
  rnf ReplicationTaskIndividualAssessment' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf replicationTaskAssessmentRunArn
      `Prelude.seq` Prelude.rnf replicationTaskIndividualAssessmentArn
      `Prelude.seq` Prelude.rnf individualAssessmentName
      `Prelude.seq` Prelude.rnf
        replicationTaskIndividualAssessmentStartDate
