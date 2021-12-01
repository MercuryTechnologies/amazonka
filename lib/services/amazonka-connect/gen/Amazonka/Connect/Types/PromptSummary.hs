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
-- Module      : Amazonka.Connect.Types.PromptSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.PromptSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the prompt.
--
-- /See:/ 'newPromptSummary' smart constructor.
data PromptSummary = PromptSummary'
  { -- | The Amazon Resource Name (ARN) of the prompt.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the prompt.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the prompt.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PromptSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'promptSummary_arn' - The Amazon Resource Name (ARN) of the prompt.
--
-- 'name', 'promptSummary_name' - The name of the prompt.
--
-- 'id', 'promptSummary_id' - The identifier of the prompt.
newPromptSummary ::
  PromptSummary
newPromptSummary =
  PromptSummary'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the prompt.
promptSummary_arn :: Lens.Lens' PromptSummary (Prelude.Maybe Prelude.Text)
promptSummary_arn = Lens.lens (\PromptSummary' {arn} -> arn) (\s@PromptSummary' {} a -> s {arn = a} :: PromptSummary)

-- | The name of the prompt.
promptSummary_name :: Lens.Lens' PromptSummary (Prelude.Maybe Prelude.Text)
promptSummary_name = Lens.lens (\PromptSummary' {name} -> name) (\s@PromptSummary' {} a -> s {name = a} :: PromptSummary)

-- | The identifier of the prompt.
promptSummary_id :: Lens.Lens' PromptSummary (Prelude.Maybe Prelude.Text)
promptSummary_id = Lens.lens (\PromptSummary' {id} -> id) (\s@PromptSummary' {} a -> s {id = a} :: PromptSummary)

instance Core.FromJSON PromptSummary where
  parseJSON =
    Core.withObject
      "PromptSummary"
      ( \x ->
          PromptSummary'
            Prelude.<$> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
      )

instance Prelude.Hashable PromptSummary where
  hashWithSalt salt' PromptSummary' {..} =
    salt' `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn

instance Prelude.NFData PromptSummary where
  rnf PromptSummary' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
