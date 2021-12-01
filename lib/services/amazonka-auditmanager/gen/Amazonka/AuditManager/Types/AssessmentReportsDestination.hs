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
-- Module      : Amazonka.AuditManager.Types.AssessmentReportsDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.AssessmentReportsDestination where

import Amazonka.AuditManager.Types.AssessmentReportDestinationType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The location in which Audit Manager saves assessment reports for the
-- given assessment.
--
-- /See:/ 'newAssessmentReportsDestination' smart constructor.
data AssessmentReportsDestination = AssessmentReportsDestination'
  { -- | The destination of the assessment report.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The destination type, such as Amazon S3.
    destinationType :: Prelude.Maybe AssessmentReportDestinationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssessmentReportsDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'assessmentReportsDestination_destination' - The destination of the assessment report.
--
-- 'destinationType', 'assessmentReportsDestination_destinationType' - The destination type, such as Amazon S3.
newAssessmentReportsDestination ::
  AssessmentReportsDestination
newAssessmentReportsDestination =
  AssessmentReportsDestination'
    { destination =
        Prelude.Nothing,
      destinationType = Prelude.Nothing
    }

-- | The destination of the assessment report.
assessmentReportsDestination_destination :: Lens.Lens' AssessmentReportsDestination (Prelude.Maybe Prelude.Text)
assessmentReportsDestination_destination = Lens.lens (\AssessmentReportsDestination' {destination} -> destination) (\s@AssessmentReportsDestination' {} a -> s {destination = a} :: AssessmentReportsDestination)

-- | The destination type, such as Amazon S3.
assessmentReportsDestination_destinationType :: Lens.Lens' AssessmentReportsDestination (Prelude.Maybe AssessmentReportDestinationType)
assessmentReportsDestination_destinationType = Lens.lens (\AssessmentReportsDestination' {destinationType} -> destinationType) (\s@AssessmentReportsDestination' {} a -> s {destinationType = a} :: AssessmentReportsDestination)

instance Core.FromJSON AssessmentReportsDestination where
  parseJSON =
    Core.withObject
      "AssessmentReportsDestination"
      ( \x ->
          AssessmentReportsDestination'
            Prelude.<$> (x Core..:? "destination")
            Prelude.<*> (x Core..:? "destinationType")
      )

instance
  Prelude.Hashable
    AssessmentReportsDestination
  where
  hashWithSalt salt' AssessmentReportsDestination' {..} =
    salt' `Prelude.hashWithSalt` destinationType
      `Prelude.hashWithSalt` destination

instance Prelude.NFData AssessmentReportsDestination where
  rnf AssessmentReportsDestination' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf destinationType

instance Core.ToJSON AssessmentReportsDestination where
  toJSON AssessmentReportsDestination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("destination" Core..=) Prelude.<$> destination,
            ("destinationType" Core..=)
              Prelude.<$> destinationType
          ]
      )
