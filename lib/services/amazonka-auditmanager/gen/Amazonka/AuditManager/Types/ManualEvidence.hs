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
-- Module      : Amazonka.AuditManager.Types.ManualEvidence
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ManualEvidence where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Evidence that is uploaded to Audit Manager manually.
--
-- /See:/ 'newManualEvidence' smart constructor.
data ManualEvidence = ManualEvidence'
  { -- | The Amazon S3 URL that points to a manual evidence object.
    s3ResourcePath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManualEvidence' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3ResourcePath', 'manualEvidence_s3ResourcePath' - The Amazon S3 URL that points to a manual evidence object.
newManualEvidence ::
  ManualEvidence
newManualEvidence =
  ManualEvidence' {s3ResourcePath = Prelude.Nothing}

-- | The Amazon S3 URL that points to a manual evidence object.
manualEvidence_s3ResourcePath :: Lens.Lens' ManualEvidence (Prelude.Maybe Prelude.Text)
manualEvidence_s3ResourcePath = Lens.lens (\ManualEvidence' {s3ResourcePath} -> s3ResourcePath) (\s@ManualEvidence' {} a -> s {s3ResourcePath = a} :: ManualEvidence)

instance Core.FromJSON ManualEvidence where
  parseJSON =
    Core.withObject
      "ManualEvidence"
      ( \x ->
          ManualEvidence'
            Prelude.<$> (x Core..:? "s3ResourcePath")
      )

instance Prelude.Hashable ManualEvidence where
  hashWithSalt salt' ManualEvidence' {..} =
    salt' `Prelude.hashWithSalt` s3ResourcePath

instance Prelude.NFData ManualEvidence where
  rnf ManualEvidence' {..} = Prelude.rnf s3ResourcePath

instance Core.ToJSON ManualEvidence where
  toJSON ManualEvidence' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("s3ResourcePath" Core..=)
              Prelude.<$> s3ResourcePath
          ]
      )
