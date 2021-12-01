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
-- Module      : Amazonka.FraudDetector.Types.Label
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.Label where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The label details.
--
-- /See:/ 'newLabel' smart constructor.
data Label = Label'
  { -- | Timestamp of when the label was last updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text,
    -- | The label ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of when the event type was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The label name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The label description.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Label' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTime', 'label_lastUpdatedTime' - Timestamp of when the label was last updated.
--
-- 'arn', 'label_arn' - The label ARN.
--
-- 'createdTime', 'label_createdTime' - Timestamp of when the event type was created.
--
-- 'name', 'label_name' - The label name.
--
-- 'description', 'label_description' - The label description.
newLabel ::
  Label
newLabel =
  Label'
    { lastUpdatedTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | Timestamp of when the label was last updated.
label_lastUpdatedTime :: Lens.Lens' Label (Prelude.Maybe Prelude.Text)
label_lastUpdatedTime = Lens.lens (\Label' {lastUpdatedTime} -> lastUpdatedTime) (\s@Label' {} a -> s {lastUpdatedTime = a} :: Label)

-- | The label ARN.
label_arn :: Lens.Lens' Label (Prelude.Maybe Prelude.Text)
label_arn = Lens.lens (\Label' {arn} -> arn) (\s@Label' {} a -> s {arn = a} :: Label)

-- | Timestamp of when the event type was created.
label_createdTime :: Lens.Lens' Label (Prelude.Maybe Prelude.Text)
label_createdTime = Lens.lens (\Label' {createdTime} -> createdTime) (\s@Label' {} a -> s {createdTime = a} :: Label)

-- | The label name.
label_name :: Lens.Lens' Label (Prelude.Maybe Prelude.Text)
label_name = Lens.lens (\Label' {name} -> name) (\s@Label' {} a -> s {name = a} :: Label)

-- | The label description.
label_description :: Lens.Lens' Label (Prelude.Maybe Prelude.Text)
label_description = Lens.lens (\Label' {description} -> description) (\s@Label' {} a -> s {description = a} :: Label)

instance Core.FromJSON Label where
  parseJSON =
    Core.withObject
      "Label"
      ( \x ->
          Label'
            Prelude.<$> (x Core..:? "lastUpdatedTime")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdTime")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable Label where
  hashWithSalt salt' Label' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` lastUpdatedTime

instance Prelude.NFData Label where
  rnf Label' {..} =
    Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf arn
