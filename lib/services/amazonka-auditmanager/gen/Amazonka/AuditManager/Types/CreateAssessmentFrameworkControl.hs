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
-- Module      : Amazonka.AuditManager.Types.CreateAssessmentFrameworkControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.CreateAssessmentFrameworkControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Control entity attributes that uniquely identify an existing control to
-- be added to a framework in Audit Manager.
--
-- /See:/ 'newCreateAssessmentFrameworkControl' smart constructor.
data CreateAssessmentFrameworkControl = CreateAssessmentFrameworkControl'
  { -- | The unique identifier of the control.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssessmentFrameworkControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'createAssessmentFrameworkControl_id' - The unique identifier of the control.
newCreateAssessmentFrameworkControl ::
  CreateAssessmentFrameworkControl
newCreateAssessmentFrameworkControl =
  CreateAssessmentFrameworkControl'
    { id =
        Prelude.Nothing
    }

-- | The unique identifier of the control.
createAssessmentFrameworkControl_id :: Lens.Lens' CreateAssessmentFrameworkControl (Prelude.Maybe Prelude.Text)
createAssessmentFrameworkControl_id = Lens.lens (\CreateAssessmentFrameworkControl' {id} -> id) (\s@CreateAssessmentFrameworkControl' {} a -> s {id = a} :: CreateAssessmentFrameworkControl)

instance
  Prelude.Hashable
    CreateAssessmentFrameworkControl
  where
  hashWithSalt
    salt'
    CreateAssessmentFrameworkControl' {..} =
      salt' `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    CreateAssessmentFrameworkControl
  where
  rnf CreateAssessmentFrameworkControl' {..} =
    Prelude.rnf id

instance Core.ToJSON CreateAssessmentFrameworkControl where
  toJSON CreateAssessmentFrameworkControl' {..} =
    Core.object
      (Prelude.catMaybes [("id" Core..=) Prelude.<$> id])
