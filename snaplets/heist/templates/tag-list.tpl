<apply template="layout">

    <bind tag="subtitle"> :: <i18n name="page.tags" /></bind>

    <h2><i18n name="tag.listHeader" /></h2>
    <ul class="all-tags-list">
        <tags>
            <li>
                <apply template="_single-tag" />
            </li>
        </tags>
    </ul>

</apply>
