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
-- Module      : Amazonka.RobOMaker.Types.WorldSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.WorldSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a world.
--
-- /See:/ 'newWorldSummary' smart constructor.
data WorldSummary = WorldSummary'
  { -- | The Amazon Resource Name (ARN) of the world.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the world was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (arn) of the world template.
    template :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (arn) of the world generation job.
    generationJob :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorldSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'worldSummary_arn' - The Amazon Resource Name (ARN) of the world.
--
-- 'createdAt', 'worldSummary_createdAt' - The time, in milliseconds since the epoch, when the world was created.
--
-- 'template', 'worldSummary_template' - The Amazon Resource Name (arn) of the world template.
--
-- 'generationJob', 'worldSummary_generationJob' - The Amazon Resource Name (arn) of the world generation job.
newWorldSummary ::
  WorldSummary
newWorldSummary =
  WorldSummary'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      template = Prelude.Nothing,
      generationJob = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the world.
worldSummary_arn :: Lens.Lens' WorldSummary (Prelude.Maybe Prelude.Text)
worldSummary_arn = Lens.lens (\WorldSummary' {arn} -> arn) (\s@WorldSummary' {} a -> s {arn = a} :: WorldSummary)

-- | The time, in milliseconds since the epoch, when the world was created.
worldSummary_createdAt :: Lens.Lens' WorldSummary (Prelude.Maybe Prelude.UTCTime)
worldSummary_createdAt = Lens.lens (\WorldSummary' {createdAt} -> createdAt) (\s@WorldSummary' {} a -> s {createdAt = a} :: WorldSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (arn) of the world template.
worldSummary_template :: Lens.Lens' WorldSummary (Prelude.Maybe Prelude.Text)
worldSummary_template = Lens.lens (\WorldSummary' {template} -> template) (\s@WorldSummary' {} a -> s {template = a} :: WorldSummary)

-- | The Amazon Resource Name (arn) of the world generation job.
worldSummary_generationJob :: Lens.Lens' WorldSummary (Prelude.Maybe Prelude.Text)
worldSummary_generationJob = Lens.lens (\WorldSummary' {generationJob} -> generationJob) (\s@WorldSummary' {} a -> s {generationJob = a} :: WorldSummary)

instance Core.FromJSON WorldSummary where
  parseJSON =
    Core.withObject
      "WorldSummary"
      ( \x ->
          WorldSummary'
            Prelude.<$> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "template")
            Prelude.<*> (x Core..:? "generationJob")
      )

instance Prelude.Hashable WorldSummary where
  hashWithSalt salt' WorldSummary' {..} =
    salt' `Prelude.hashWithSalt` generationJob
      `Prelude.hashWithSalt` template
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` arn

instance Prelude.NFData WorldSummary where
  rnf WorldSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf generationJob
      `Prelude.seq` Prelude.rnf template
      `Prelude.seq` Prelude.rnf createdAt
